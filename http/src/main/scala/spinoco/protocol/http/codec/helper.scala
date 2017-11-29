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

  val trimmedAsciiToken = ignoreWS ~> asciiToken <~ ignoreWS

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




  /** string encoded // decoded as ASCII **/
  val asciiString: Codec[String] = string(StandardCharsets.US_ASCII)

  /** string encoded // decoded as UTF8 **/
  val utf8String: Codec[String] = string(StandardCharsets.UTF_8)



  /** utf8 string that is trimmed of WS **/
  val trimmedUtf8String: Codec[String] = utf8String.xmap(_.trim, _.trim)

  private val quotedChars: Set[Char] = "()<>@.,;:\\/[]?={} \t".toSet



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

  /**
    * Encodes as supplied ascii string constant. Drops any leading whitespace if any.
    * @param const        Constant to encode // decode
    * @param ignoreCase   When true, the decoding ignores case
    * @return
    */
  def asciiConstant(const: String, ignoreCase: Boolean = true): Codec[Unit] = {
    val example = ByteVector.view(const.getBytes)

    new Codec[Unit] {
      def decode(bits: BitVector): Attempt[DecodeResult[Unit]] = {
        val present = bits.bytes.dropWhile(_.toChar.isWhitespace)

        if (present.take(example.size) == example) Attempt.successful(DecodeResult((), present.drop(example.size).bits))
        else if (! ignoreCase || present.size != example.size) Attempt.failure(Err(s"Expected $const, got ${bits.take(example.size).decodeUtf8}"))
        else {
          present.take(example.size).decodeAscii match {
            case Left(ex) => Attempt.failure(Err(s"Invalid coding of input string: ${ex.getMessage}"))
            case Right(s) =>
              if (const.equalsIgnoreCase(s)) Attempt.successful(DecodeResult((), present.drop(example.size).bits))
              else Attempt.failure(Err(s"Expected $const, got ${bits.take(example.size).decodeUtf8}"))
          }

        }

      }
      def encode(value: Unit): Attempt[BitVector] = Attempt.successful(example.bits)
      def sizeBound: SizeBound = SizeBound.exact(example.size)
    }
  }

  /** codec that decodes until whitespace character is found. Encodes as ascii string **/
  def asciiStringNoWs: Codec[String] =
    asciiStringUntil(! _.isWhitespace)

  def asciiStringUntil(f: Char => Boolean): Codec[String] = {
    bytesUntil(b => f(b.toChar)).exmap (
      bs => asciiString.decodeValue(bs.bits)
      , s => asciiString.encode(s).map(_.bytes)
    )
  }

  /** codec that strips all whitespace, and encodes as supplied string. At least one whitespace is mandatory. **/
  def whitespace(encodeAs: String = " "): Codec[Unit] = {
    val content = ByteVector.view(encodeAs.getBytes)
    new Codec[Unit] {
      def sizeBound: SizeBound = SizeBound.unknown
      def encode(value: Unit): Attempt[BitVector] = Attempt.successful(content.bits)
      def decode(bits: BitVector): Attempt[DecodeResult[Unit]] = {
        val h = bits.bytes.takeWhile(_.toChar.isWhitespace)
        if (h.size == 0) Attempt.failure(Err(s"Expected whitespace, got ${bits.decodeUtf8}"))
        else {
          val t = bits.drop(h.size*8)
          Attempt.successful(DecodeResult((), t))
        }
      }
    }
  }

  /** codec that succeeds, iff star (*) is present **/
  val starCodec: Codec[Unit] = trimmedAsciiToken.exmap(
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

    (ignoreWS ~> asciiString).exmap(
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


  val base64Encoded: Codec[ByteVector] = {
    utf8String.exmap(
      s => ByteVector.fromBase64(s.trim).map(Attempt.successful).getOrElse(Attempt.failure(Err(s"Invalid BASE64 encoding: $s")))
      , bv => Attempt.successful(bv.toBase64)
    )
  }

  /** codec that decodes to Some(a) when codec decodes or None, when there are no nonempty characters left **/
  def orEmpty[A](codec:Codec[A]):Codec[Option[A]] = new Codec[Option[A]] {
    def decode(bits: BitVector): Attempt[DecodeResult[Option[A]]] = {
      codec.decode(bits).map(_.map(Some(_))) recoverWith {
        case err =>
          if (bits.bytes.dropWhile(_.toChar.isWhitespace).isEmpty) Attempt.successful(DecodeResult(None, BitVector.empty))
          else Attempt.failure(err)
      }
    }

    def encode(value: Option[A]): Attempt[BitVector] = {
      value match {
        case None => Attempt.successful(BitVector.empty)
        case Some(a) => codec.encode(a)
      }
    }

    def sizeBound: SizeBound = codec.sizeBound.atMost
  }

  /** decodes as utf8 string, that will return string until pattern is matched **/
  def utf8StringUntil(until: String):Codec[String] = new Codec[String] {
    // todo byke this more effective by decoding string as we match the sample
    def decode(bits: BitVector): Attempt[DecodeResult[String]] = {
      utf8.decode(bits).flatMap { case dr@DecodeResult(s,rem) =>
       s.indexOf(until) match {
         case idx if idx < 0 => Attempt.successful(dr)
         case idx =>
           val (h, t) = s.splitAt(idx)
           Attempt.successful(DecodeResult(h, BitVector.view(t.getBytes) ++ rem))
       }
      }
    }

    def encode(value: String): Attempt[BitVector] =
      utf8.encode(value)

    def sizeBound: SizeBound = SizeBound.unknown
  }



}
