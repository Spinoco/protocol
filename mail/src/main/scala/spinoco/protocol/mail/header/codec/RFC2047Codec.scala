package spinoco.protocol.mail.header.codec

import java.nio.CharBuffer
import java.nio.charset.Charset

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._
import spinoco.protocol.common.util._

import scala.annotation.tailrec

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

    /*
     * Decodes individual RFC 2047 words by skipping any whitespace and stripping off =? and ?= delimiters
     *
     * Returned boolean is true, if the encoding type is set to `Q` or `q`
     *
     */
    def decodeRFC2047(decode: String): Attempt[String] = {

      def decodeLine(line: String): Attempt[String] = {
        val segment = line.trim
        if (segment.startsWith("=?") && segment.endsWith("?=")) {
          val toDecode = segment.slice(2, segment.length - 2)
          val parts = toDecode.split('?')
          if (parts.length != 3) Attempt.failure(Err(s"RFC 2047 word must start with =? and end with ?= and have 3 parts separated by ? : ${parts.toList} "))
          else {
            attempt(Charset.forName(parts(0))) flatMap { chs =>
                parts(1).toUpperCase match {
                  case "Q" => decodeQ(chs, parts(2))
                  case "B" => decodeB(chs, parts(2))
                  case other => Attempt.failure(Err(s"RFC 2047 Invalid encoding $other : $toDecode "))
                }
              }
          }
        } else {
          Attempt.failure(Err(s"RFC 2047 word must start with =? and end with ?=: $line "))
        }
      }

      @tailrec
      def go(lines: Seq[String], acc: String): Attempt[String] = {
        lines.headOption match {
          case Some(line) => decodeLine(line) match {
            case Attempt.Successful(decoded) => go(lines.tail, acc+decoded)
            case Attempt.Failure(err) => Attempt.failure(err)
          }

          case None => Attempt.successful(acc)
        }
      }

      go(decode.split('\n'), "")
    }


    def encodeRFC2047(encode: String): Attempt[String] = {
      if (encode.forall { c => AsciiEncoder.canEncode(c)  && ! c.isControl && c != '?' }) Attempt.successful(encode)
      else {
        @tailrec
        def go(remains: String, buff: String, acc: String): Attempt[String] = {
          remains.headOption match {
            case Some(ch) =>
              val encoded: String = ch match {
                case ' ' => "_"
                case '_' => "=5F"
                case '?' => "=3F"
                case '=' => "=3D"
                case c =>
                  if (AsciiEncoder.canEncode(c) && !c.isControl && c != '?') c.toString
                  else ByteVector.view(UTF8Encoder.encode(CharBuffer.wrap(Array(c)))).toHex.toUpperCase.grouped(2).flatMap { "=" + _ }.mkString
              }
              if (buff.isEmpty) go(remains.tail, "=?UTF-8?Q?" + encoded, acc)
              else if (buff.length + encoded.length > MaxLineSize) if (acc.nonEmpty) go(remains.tail, "", acc + "\r\n " + buff + "?=") else  go(remains.tail, "", buff + "?=")
              else go(remains.tail, buff + encoded, acc)

            case None =>
              if (buff.isEmpty) Attempt.successful(acc)
              else if (acc.nonEmpty) Attempt.successful(acc + "\r\n " + buff + "?=") else Attempt.successful(buff + "?=")
          }
        }

        go(encode, "", "")
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
