package spinoco.protocol.http.codec

import java.nio.charset.StandardCharsets
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.common.util.attempt

import scala.annotation.tailrec

/**
  * Created by pach on 13/01/17.
  */
object helper {

  val comma = ByteVector(',')
  val SP = ByteVector(' ')
  val comma_SP = ByteVector(',',' ')

  val colon = ByteVector(':')
  val semicolon = ByteVector(';')
  val semicolon_SP = ByteVector(';',' ')
  val dash = ByteVector('-')

  val slash = ByteVector('/')
  val star = ByteVector('*')
  val qPar = ByteVector('q','=')

  val amp = ByteVector('&')

  val _equal = ByteVector('=')

  val `://` = ByteVector(':','/','/')

  val crlf = ByteVector('\r','\n')

  /**
    * Codec for decoding comma delimited parameters from header.
    * When encoding, the comma is always followed by [SP]
    *
    * When decoding, the parameters are tested if they are quoted strings and if yes,
    * the comma is skipped if within quotes
    *
    */
  def commaDelimited[A](valueCodec: Codec[A]): Codec[List[A]] =
    delimitedBy(comma, comma_SP, valueCodec)

  def commaDelimitedMin[A](codec:Codec[A], min:Int):Codec[List[A]] = {
    def guard(items: List[A]):Attempt[List[A]] = {
      if (items.size < min) Attempt.failure(Err(s"Required $min parameters, but got $items"))
      else Attempt.successful(items)
    }
    commaDelimited(codec).exmap(guard,guard)
  }

  /**
    * Codec for decoding delimited parameters from header.
    * When encoding, the encodeBy is used to encode `A` delimiter
    *
    * When decoding, the parameters are tested if they are quoted strings and if yes,
    * the `by` is skipped if within quotes
    *
    */
  def delimitedBy[A](by: ByteVector, encodeBy: ByteVector, valueCodec: Codec[A]): Codec[List[A]] = {
    listMultiplexed(
      _ ++ encodeBy.bits ++ _
      , bits => splitByQuoted(by, bits)
      , valueCodec)
  }

  /** codec for qValue in multiple parameter lists**/
  val qValueCodec: Codec[Float] = {
    constant(qPar) ~> floatAsString
  }

  /** splits supplied content by supplied delimiter. Matches delimiter by each byte **/
  private def splitBy(delimiter: ByteVector, content: BitVector):(BitVector, BitVector) = {
    val bs = content.bytes
    bs.indexOfSlice(delimiter) match {
      case -1 => content -> BitVector.empty
      case at =>
        val (h,t) = bs.splitAt(at)
        h.bits -> t.drop(delimiter.size).bits
    }
  }

  /** splits by delimiter, honoring quoted commas **/
  def splitByQuoted(delimiter: ByteVector, content:BitVector):(BitVector, BitVector) = {
    val bs = content.bytes
    @tailrec
    def go(rem: ByteVector, inQuote: Boolean):(BitVector, BitVector) = {
      if (rem.isEmpty) (content, BitVector.empty)
      else if (rem.head == '"') {
        if (inQuote) go(rem.tail, inQuote = false)
        else go(rem.tail, inQuote = true)
      } else {
        if (inQuote) go(rem.tail, inQuote = true)
        else {
          if (! rem.startsWith(delimiter)) go(rem.tail, inQuote = false)
          else {
            val t = rem.drop(delimiter.length)
            val h = bs.take(bs.length - t.length - delimiter.length)
            h.bits -> t.bits
          }
        }
      }
    }
    go (bs, inQuote = false)

  }


  def splitByWS(content: BitVector): (BitVector, BitVector) = {
    val hws = content.bytes.dropWhile(_.toChar.isWhitespace)
    val h = hws.takeWhile {! _.toChar.isWhitespace }
    val t = hws.drop(h.size).dropWhile(_.toChar.isWhitespace)
    (h.bits, t.bits)
  }



  /**
    * A codec, that allows to encode/decode parametrized values `A` by parameter `B`.
    * Parameter is optional and is discriminated by `discriminator`
    */
  def parametrized[A,B](discriminator:ByteVector, aCodec:Codec[A], bCodec:Codec[B]):Codec[(A,Option[B])] =
    parametrized2(discriminator,discriminator,aCodec, bCodec)




  def parametrized2[A,B](discriminator:ByteVector, encodingDiscriminator: ByteVector, aCodec:Codec[A], bCodec:Codec[B]):Codec[(A,Option[B])] = {
    parametrizedN(discriminator, encodingDiscriminator, aCodec, bCodec).exmap(
      { case (a, bl) =>
        if (bl.size <= 1) Attempt.successful(a -> bl.headOption)
        else Attempt.failure(Err(s"Only one parameter allowed, got $bl"))
      }
      , { case (a, bo) => Attempt.successful(a -> bo.toList) }
    )
  }

  /**
    * A codec, that allows to encode/decode parametrized values `A` by optional parameters of `B`.
    * Parameter is optional (may be N), and is discriminated by `discriminator`
    *
    * furthermore if the discriminator is found in quotes (") it is ignored until cloiong quotes are found
    */
  def parametrizedN[A,B](discriminator:ByteVector, encodingDiscriminator: ByteVector, aCodec:Codec[A], bCodec:Codec[B]):Codec[(A,List[B])] = {
    def encode(in:(A, List[B])):Attempt[List[BitVector]] = {
      in match {
        case (a, bl) => aCodec.encode(a).flatMap { av =>
          @tailrec
          def go(acc: List[BitVector], rem: List[B]):Attempt[List[BitVector]] = {
            rem.headOption match {
              case None => Attempt.successful(acc)
              case Some(b) => bCodec.encode(b) match {
                case Attempt.Successful(bv) => go(acc :+ bv, rem.tail)
                case failure: Attempt.Failure => failure
              }
            }
          }
          go(List.empty, bl).map { av +: _ }
        }
      }
    }

    def decode(segments: List[BitVector]):Attempt[(A, List[B])] = {
      segments match {
        case List() => Attempt.failure(Err(s"Expected value with optional parameter, got none"))
        case a :: tail =>
          aCodec.decodeValue(a).flatMap { av =>
            @tailrec
            def go(acc:List[B], rem: List[BitVector]):Attempt[List[B]] = {
              rem.headOption match {
                case None => Attempt.successful(acc)
                case Some(b) => bCodec.decodeValue(b) match {
                  case Attempt.Successful(bv) => go(acc :+ bv, rem.tail)
                  case failure: Attempt.Failure => failure
                }
              }
            }
            go(List.empty, tail).map { av -> _ }
          }
      }
    }


    listMultiplexed(
      _ ++ encodingDiscriminator.bits ++ _
      , bits => splitByQuoted(discriminator, bits)
      , bitsWsRemoved
    ).exmap(decode,encode)
  }


  /**
    * Tuple A, B separated by `discriminator`. This will first search for discriminator, and if match
    * will apply `A` codec nad `B` codec to remaining bits, if any.
    */
  def tuple[A, B](discriminator:ByteVector, acodec:Codec[A], bCodec:Codec[B]):Codec[(A, B)] = {
    def encode(in:(A, B)):Attempt[List[BitVector]] = {
      in match { case (a, b) =>
        acodec.encode(a).flatMap { av => bCodec.encode(b).map(bv => List(av,bv)) }
      }
    }

    def decode(segments: List[BitVector]):Attempt[(A, B)] = {
      segments match {
        case a :: b :: Nil => acodec.decodeValue(a).flatMap { av => bCodec.decodeValue(b).map(av -> _) }
        case other => Attempt.failure(Err(s"Expected tuple, got ${other.map(_.decodeAscii)}"))
      }
    }

    listMultiplexed(
      _ ++ discriminator.bits ++ _
      , bits => splitBy(discriminator, bits)
      , bitsWsRemoved).exmap(decode,encode)
  }

  /** string encoded // decoded as ASCII **/
  val asciiString: Codec[String] = string(StandardCharsets.US_ASCII)

  /** string encoded // decoded as UTF8 **/
  val utf8String: Codec[String] = string(StandardCharsets.UTF_8)

  /** ascii string that is trimmed of WS **/
  val trimmedAsciiString: Codec[String] = asciiString.xmap(_.trim, _.trim)

  /** utf8 string that is trimmed of WS **/
  val trimmedUtf8String: Codec[String] = utf8String.xmap(_.trim, _.trim)

  private val quotedChars: Set[Char] = "()<>@.,;:\\/[]?={} \t".toSet

  /** coder for strings that may be quoted **/
  val quotedString: Codec[String] = {
    utf8String.xmap(
      s => {
        val st = s.trim
        if (st.startsWith("\"") && s.endsWith("\"")) st.drop(1).dropRight(1)
        else st
      }
      , s => {
        if (s.exists(quotedChars.contains)) "\"" + s + "\"" else s
      }
    )
  }

  val alwaysQuotedUtf8String: Codec[String] = {
    utf8String.exmap(
      s => {
        val st = s.trim
        if (st.startsWith("\"") && s.endsWith("\"")) Attempt.successful(st.drop(1).dropRight(1))
        else Attempt.failure(Err(s"String must be quoted: $s"))
      }
      , s => Attempt.successful( "\"" + s + "\"" )
    )
  }

  /** encodes by supplied constant. Whitespace is ignored **/
  def asciiConstant(const:String): Codec[Unit] = {
    val example = BitVector.view(const.getBytes)

    new Codec[Unit] {
      def decode(bits: BitVector): Attempt[DecodeResult[Unit]] = {
        val present = bits.bytes.dropWhile(_.toChar.isWhitespace).bits
        if (present.take(example.size) == example) Attempt.successful(DecodeResult((), present.drop(example.size)))
        else Attempt.failure(Err(s"Expected $const, got ${bits.take(example.size).decodeUtf8}"))
      }
      def encode(value: Unit): Attempt[BitVector] = Attempt.successful(example)
      def sizeBound: SizeBound = SizeBound.exact(example.size)
    }
  }

  /** codec that strips all whitespace, and encodes as supplied string **/
  def whitespace(encodeAs:String = " "): Codec[Unit] = {
    val content = ByteVector.view(encodeAs.getBytes)
    bytesUntil(_.toChar.isWhitespace).xmap(_ => (), _ => content)
  }

  /** codec that succeeds, iff star (*) is present **/
  val starCodec: Codec[Unit] = trimmedAsciiString.exmap(
    s => if (s == "*") Attempt.successful(()) else Attempt.failure(Err("Expected *"))
    , _ => Attempt.successful("*")
  )


  private val dateFormatRFC1123 = DateTimeFormatter.RFC_1123_DATE_TIME

  /** encodes // decodes time by http time format **/
  val httpDateTimeCodec: Codec[LocalDateTime] = {
    def decode(s:String):Attempt[LocalDateTime] = {
      // todo: support for non rfc 1213 formats
       attempt { ZonedDateTime.from(dateFormatRFC1123.parse(s)).toLocalDateTime }
    }

    trimmedAsciiString.exmap(
      decode
      , ldt => Attempt.successful(localDateTime2String(ldt))
    )
  }

  private val GMTZone = ZoneId.of("GMT")

  /** RFC 1123 date 2 string **/
  def localDateTime2String(ldt: LocalDateTime):String = {
    //ldt.toString
    dateFormatRFC1123.format(ldt.atZone(GMTZone))
  }

}
