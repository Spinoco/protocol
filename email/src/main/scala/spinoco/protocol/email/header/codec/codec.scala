package spinoco.protocol.email.header

import java.nio.CharBuffer
import java.nio.charset.Charset

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._
import shapeless.tag
import shapeless.tag.@@
import spinoco.protocol.common.util._
import spinoco.protocol.common.codec._

import scala.annotation.tailrec

/**
  * Created by pach on 17/10/17.
  */
package object codec {


  private val AsciiEncoder = Charset.forName("ASCII").newEncoder()
  private val UTF8Encoder = Charset.forName("UTF-8").newEncoder()

  private[codec] val `=?` = ByteVector.view("=?".getBytes)
  private[codec] val `?=` = ByteVector.view("?=".getBytes)
  private[codec] val `?` = ByteVector.view("?".getBytes)
  private[codec] val `,`: ByteVector = ByteVector.view(",".getBytes)
  private[codec] val `, `: ByteVector = ByteVector.view(", ".getBytes)
  private[codec] val foldingComma: ByteVector = ByteVector.view(",\r\n ".getBytes)
  private[codec] val cfws : ByteVector = ByteVector.view("\r\n ".getBytes)
  private[codec] val `=` : ByteVector = ByteVector.view("=".getBytes)

  /**
    * According to RFC 2047 decodes non-ascii text. Source must be ascii.
    *
    * If the source starts with =? and ands with ?=, the this is considered text
    * to be encoded by RFC 2047 and will be decoded accordingly.
    *
    * Otherwise this will yield to failure. According to rfc this shall then ignore any display of it and act as empty string.
    *
    * If invalid format is encountered when decoding the string this will report failure.
    *
    * @param from  Source, Ascii string.
    * @return
    */
  def decodeNonAscii(from: String): Attempt[String] = {
    // todo: implement max size of 75 chars.
    if (from.startsWith("=?") && from.endsWith("?=")) {
      val parts = from.drop(2).dropRight(2).split('?')
      if (parts.size != 3) Attempt.failure(Err(s"Invalid rfc 2047 format: expected =?Encoding?format?text?= got $from"))
      else {
        val chsetName = parts(0)
        val encoding = parts(1)

        if (! Charset.isSupported(chsetName)) Attempt.failure(Err(s"Unsupported character set: $chsetName"))
        else {
          attempt(Charset.forName(chsetName)) flatMap { chset =>
            if (encoding.equalsIgnoreCase("Q")) {
              @tailrec
              def go(remain: String, acc: String): Attempt[String] = {
                if (remain.isEmpty) Attempt.successful(acc)
                else if (remain(0) == '=') {
                  @tailrec
                  def decodeHex(toGo: String, bytes: String): Attempt[(String, String)] = {
                    if (toGo.nonEmpty && toGo(0) == '=') {
                      val next = toGo.slice(1, 3)
                      if (next.size == 2) decodeHex(toGo.drop(3), bytes + next)
                      else Attempt.failure(Err(s"Invalid byte specification, requires 2 chars, got: $next"))
                    } else {
                      if (bytes.isEmpty) Attempt.successful((toGo, ""))
                      else {
                        ByteVector.fromHex(bytes.toLowerCase) match {
                          case None => Attempt.failure(Err(s"Invalid byte specificate: ${bytes} : $from"))
                          case Some(bs) => attempt(chset.decode(bs.toByteBuffer).toString) map { (toGo, _) }
                        }
                      }
                    }
                  }
                  decodeHex(remain, "") match {
                    case Attempt.Successful((n, append)) =>
                      go(n, acc + append)
                    case Attempt.Failure(err) => Attempt.Failure(err)
                  }
                } else {
                  val chunk = remain.takeWhile(c => c != '=')
                  go(remain.drop(chunk.size), acc + chunk.replace('_', ' '))
                }
              }
              go(parts(2), "")
            } else if (encoding.equalsIgnoreCase("B")) {
              ByteVector.fromBase64(parts(2)) match {
                case None => Attempt.failure(Err(s"Invalid base 64 encoding encountered : ${parts(2)} : $from"))
                case Some(bytes) => attempt(chset.decode(bytes.toByteBuffer)).map(_.toString)
              }
            } else Attempt.failure(Err(s"Invalid encoding specified supported Q/B found: $encoding"))
          }

        }
      }
    } else Attempt.successful(from)
  }


  /**
    * Reverse for `decodeNonAscii` that allows to encode non-ascii characters according to RFC 2047.
    *
    * @param from source, nonAscii text. If this text contains only ascii characters it is returned as is.

    */
  def encodeNonAscii(from: String): String = {
    if (from.exists(ch => !AsciiEncoder.canEncode(ch) || ch == '?' || ch.isControl)) {
      // encode according to RFC
      // we do prefer `Q` notation as it is better human readable
      val transform =
      from.flatMap {
        case ' ' => "_"
        case '_' => "=5F"
        case '?' => "=3F"
        case c =>
          if (AsciiEncoder.canEncode(c) && ! c.isControl ) c.toString
          else ByteVector.view(UTF8Encoder.encode(CharBuffer.wrap(Array(c)))).toHex.toUpperCase.grouped(2).flatMap { "=" + _ }
      }
      "=?utf-8?Q?" + transform + "?="

    } else from

  }


  /**
    * Creates a codec, that decodes list of comma separated values.
    *
    * There must be at least one `A` decoded for this to succeed
    *
    * @param fold If true, then folding whitespace is inserted while encoding.
    */
  def commaSeparated[A](codec: Codec[A], fold: Boolean): Codec[(A, List[A])] = {
    val encodeDelimiter = if (fold) foldingComma else `,`
    delimitedBy(`,`, encodeDelimiter, codec) exmap(
      la => {
        if (la.isEmpty) Attempt.failure(Err("No element provided, expecting one"))
        else Attempt.successful((la.head, la.tail))
      }
      , { case (a, la) => Attempt.successful(a +: la) }
    )
  }

  /**
    *  Decodes `A` where the individual items are separated by folding whitespace as per RFC.
    *  Encodes by inserting `\r\n ` (CFWS) between individual items.
    *
    *  Individual `A` encodings may not contain whitespace character.
    *
    * @param codec
    * @tparam A
    * @return
    */
  def cfwsSeparated[A](codec: Codec[A]): Codec[(A, List[A])] = {

    val listCodec = listDelimited(cfws.bits, codec)

    new Codec[(A, List[A])] {
      def decode(bits: BitVector): Attempt[DecodeResult[(A, List[A])]] = {

        @tailrec
        def go(bytes: ByteVector, acc: Vector[A]): Attempt[Vector[A]] = {
          val start = bytes.dropWhile { b => b.toChar.isWhitespace }
          if (start.isEmpty) Attempt.successful(acc)
          else {
            val toDecode = start.takeWhile { b => !b.toChar.isWhitespace }
            val next = start.drop(toDecode.length).dropWhile { b => b.toChar.isWhitespace }
            codec.decode(toDecode.bits) match {
              case Attempt.Successful(rslt) =>
                if (rslt.remainder.nonEmpty) Attempt.failure(Err(s"Decoded successfully, but remainder was not processed: $rslt"))
                else go(next, acc :+ rslt.value)
              case Attempt.Failure(err) => Attempt.failure(err)
            }
          }
        }

        go(bits.bytes, Vector.empty) flatMap { decoded =>
          if (decoded.isEmpty) Attempt.failure(Err("Expected at least one `A` got none"))
          else Attempt.successful(DecodeResult((decoded.head, decoded.tail.toList), BitVector.empty))
        }

      }

      def encode(value: (A, List[A])): Attempt[BitVector] =
        listCodec.encode(value._1 +: value._2)

      def sizeBound: SizeBound =
        SizeBound.unknown
    }
  }


  private val AtomAcceptChars: Set[Char] = Set(
    '!', '#', '$', '%', '&', ''', '*', '+', '-', '/', '=', '?', '^', '_', '`', '{', '}', '|', '~'
  )

  def isAtomChar(c: Char): Boolean =
    c.isLetterOrDigit || AtomAcceptChars.contains(c)

  def isAtomDotChar(c: Char): Boolean =
    c.isLetterOrDigit || AtomAcceptChars.contains(c) || c == '.'


  /** string that is encoded as atom **/
  val atomString: Codec[String] = {
    def verifyAtom(s: String): Attempt[String] = {
      if (s.forall(isAtomChar)) Attempt.successful(s)
      else Attempt.failure(Err(s"String is not atom: $s"))
    }
    utf8.exmap(verifyAtom, verifyAtom)
  }

  /** quoted string **/
  val quotedString: Codec[String] = {
    utf8.exmap(
      s => {
        val s0 = s.trim
        if (s0.size > 1 && s0.head == '"' && s0.last == '"') {
          Attempt.successful(s0.init.tail)
        } else Attempt.failure(Err(s"Expected Quoted string, got: $s"))
      }
      , s => Attempt.successful('"' + s + '"')
    )
  }

  // keyword in RFC 5322
  val keyword = choice(atomString, quotedString)


  val msgIdCodec: Codec[String @@ MessageId] = {
    def decode(s0: String): Attempt[String @@ MessageId] = {
      val s = s0.trim
      if (s.length > 1 && s.head == '<' && s.last == '>' ) {
        val content = s.tail.init
        val leftRight = content.split('@')
        if (leftRight.length != 2) Attempt.failure(Err(s"Invalid message d: $s0"))
        else {
          if (leftRight(0).exists(c => !isAtomDotChar(c)) || leftRight(1).exists(c => !isAtomDotChar(c))) Attempt.failure(Err(s"Invalid message id, id is not atomchar per RFC: $content"))
          else Attempt.successful(tag[MessageId](leftRight(0) +"@" + leftRight(1)))
        }
      } else Attempt.failure(Err(s"Invalid message Id: $s0"))
    }

    def encode(id: String @@ MessageId): Attempt[String] =
      Attempt.successful("<" + id + ">")

    ascii.exmap (decode, encode)
  }





}
