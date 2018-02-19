package spinoco.protocol.mail.header.codec

import java.nio.CharBuffer
import java.nio.charset.Charset

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._
import spinoco.protocol.common.util._

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Groups

/**
  * Created by pach on 23/10/17.
  */
object RFC2047Codec {

  /**
    * RFC 2047 codec.
    *
    * This will take a bytes that are decoded as asciss string and then, this will search for the RFC2047 encoding
    * indicator (=?), if that is found, this will start to decode the string as RFC 2047 content, psoobly separated by FWS.
    *
    * However if the encoded string cannot be decoded, this will, instead of failing return the Ascii string.
    *
    * When encoding, this will encode string to RFC2047 if the characters supplied cannot be encoded by plain string.
    * If the characters encoded are exceeding the 75 characters per line, this will result in mulitple RFC2047 encoded words,
    * separated by FWS.
    *
    */
  val codec: Codec[String] = {
    val AsciiEncoder = Charset.forName("ASCII").newEncoder()
    val UTF8Encoder = Charset.forName("UTF-8").newEncoder()
    val MaxLineSize = 75 // max size of line before put FWS and new word when encoding
    val EncodedWord = "=\\?([^\\?]+)\\?([^\\?]+)\\?([^\\?]*)\\?=".r //"=?" charset "?" encoding "?" encoded-text "?="

    /*
     * Decodes individual RFC 2047 words by skipping any whitespace and stripping off =? and ?= delimiters
     *
     * Returned boolean is true, if the encoding type is set to `Q` or `q`
     *
     */
    def decodeRFC2047(decode: String): Attempt[String] = {

      def decodeWord(word: Regex.Match): Attempt[String] = {
        word match {
          case Groups(charset, encoding, text) =>
            attempt(Charset.forName(charset)) flatMap { chs =>
              encoding.toUpperCase match {
                case "Q" => decodeQ(chs, text)
                case "B" => decodeB(chs, text)
                case other => Attempt.failure(Err(s"RFC 2047 Invalid encoding $other : $word "))
              }
            }

          case _ =>
            Attempt.failure(Err(s"RFC 2047 word must start with =? and end with ?= and have 3 parts separated by ? : ${word.subgroups} "))
        }
      }

      @tailrec
      def go(words: Seq[Regex.Match], acc: String): Attempt[String] = {
        words.headOption match {
          case Some(word) => decodeWord(word) match {
            case Attempt.Successful(decoded) => go(words.tail, acc + decoded)
            case Attempt.Failure(err) => Attempt.failure(err)
          }

          case None => Attempt.successful(acc)
        }
      }

      go(EncodedWord.findAllMatchIn(decode).toSeq, "")
    }


    def encodeRFC2047(encode: String): Attempt[String] = {
      if (encode.forall { c => AsciiEncoder.canEncode(c)  && ! c.isControl && c != '?' }) Attempt.successful(encode)
      else {
        @tailrec
        def go(remains: String, buff: String, acc: String, highSurrogate: Option[Char]): Attempt[String] = {
          remains.headOption match {
            case Some(ch) =>
              val encoded: Option[String] = ch match {
                case ' ' => Some("_")
                case '_' => Some("=5F")
                case '?' => Some("=3F")
                case '=' => Some("=3D")
                case c =>
                  if (c.isHighSurrogate) None
                  else if (AsciiEncoder.canEncode(c) && !c.isControl && c != '?') Some(c.toString)
                  else Some(ByteVector.view(UTF8Encoder.encode(CharBuffer.wrap(highSurrogate.toArray ++ Array(c)))).toHex.toUpperCase.grouped(2).flatMap { "=" + _ }.mkString)
              }

              encoded match {
                case None => go(remains.tail, buff, acc, Some(ch))
                case Some(encodedChar) =>
                  if (buff.isEmpty) go(remains.tail, "=?UTF-8?Q?" + encodedChar, acc, None)
                  else if (buff.length + encodedChar.length > MaxLineSize) if (acc.nonEmpty) go(remains.tail, "", acc + "\r\n " + buff + "?=", None) else  go(remains.tail, "", buff + "?=", None)
                  else go(remains.tail, buff + encodedChar, acc, None)
              }

            case None =>
              if (buff.isEmpty) Attempt.successful(acc)
              else if (acc.nonEmpty) Attempt.successful(acc + "\r\n " + buff + "?=") else Attempt.successful(buff + "?=")
          }
        }

        go(encode, "", "", None)
      }
    }



    new Codec[String] {
      def encode(value: String): Attempt[BitVector] =
        encodeRFC2047(value) flatMap { encoded =>
          ascii.encode(encoded)
        }

      def decode(bits: BitVector): Attempt[DecodeResult[String]] = {
        ascii.decode(bits) flatMap { case (r@DecodeResult(s, remainder)) =>
          if (! s.trim.startsWith("=?")) Attempt.successful(r)
          else Attempt.successful {
            decodeRFC2047(s.trim).fold(_ => r, s0 => DecodeResult(s0, remainder))
          }
        }
      }
      def sizeBound: SizeBound = SizeBound.unknown
    }

  }

  /**
    * From the supplied string decodes string via charset supplied in RFC2047 Q format
    */
  def decodeQ(chs: Charset, s : String): Attempt[String] = {
    @tailrec
    def go(remains: String, hex: String, acc: String): Attempt[String] = {
      remains.headOption match {
        case Some(c) =>
          if (c != '=') {
            // if there is something in hex, lets decode it, otherwise lets just add the char
            val dc = if (c == '_') ' ' else c
            if (hex.isEmpty) go(remains.tail, hex, acc :+ dc)
            else {
              (Attempt.fromOption(ByteVector.fromHex(hex.toLowerCase), Err(s"Invalid hex encoding: $hex")) flatMap { bytes =>
                attempt(chs.decode(bytes.toByteBuffer)) map { _.toString }
              }) match {
                case Attempt.Successful(decoded) => go(remains.tail, "", acc + decoded + dc)
                case Attempt.Failure(err) => Attempt.failure(err)
              }
            }
          } else {
            if (remains.length < 3) Attempt.failure(Err(s"Not enough characters to decode ($remains from $s)"))
            else go(remains.drop(3), hex + remains.tail.take(2), acc)
          }

        case None => Attempt.successful(acc)
      }
    }

    go(s, "", "")
  }


  def decodeB(chs: Charset, s: String): Attempt[String] = {
    Attempt.fromOption(ByteVector.fromBase64(s), Err(s"Invalid base64 encoding: $s")) flatMap { bs =>
      attempt { chs.decode(bs.toByteBuffer) } map { _.toString }
    }
  }


}
