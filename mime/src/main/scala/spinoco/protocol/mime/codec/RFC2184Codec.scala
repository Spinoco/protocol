package spinoco.protocol.mime.codec

import java.net.{URLDecoder, URLEncoder}
import java.nio.CharBuffer
import java.nio.charset.Charset

import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import spinoco.protocol.common.codec._
import spinoco.protocol.common.util._

/**
  * Created with IntelliJ IDEA.
  * User: raulim
  * Date: 30.10.18
  */

/**
  * @see https://tools.ietf.org/html/rfc2184
  */
object RFC2184Codec {

  val codec: Codec[Vector[(String, String)]] = {
    new Codec[Vector[(String, String)]] {
      def decode(bits: BitVector): Attempt[DecodeResult[Vector[(String, String)]]] = {
        impl.decodeParameters(bits, Vector.empty)
      }

      def encode(value: Vector[(String, String)]): Attempt[BitVector] = {
        impl.encodeRFC2184(value)
      }

      def sizeBound: SizeBound = SizeBound.unknown
    }
  }

  object impl {
    val maxValueLength = 78
    val AsciiEncoder = Charset.forName("ASCII").newEncoder()

    val attribute: Codec[String] = {
      takeWhileChar(asciiToken)('*', '=')
    }

    val attributeExtension: Codec[(Option[Int], Boolean)] = {
      maybe(constantString1("*") ~> digits) ~ maybe(constantString1("*")).xmap[Boolean](_.isDefined, {
        case true => Some(())
        case false => None
      }) <~ constantString1("=")
    }

    val attributeWithExtension = maybe(constantString1(";")).unit(None) ~> ignoreWS ~> (attribute ~ attributeExtension)

    // [charset]'[language]'
    val charsetAndLang = {
      (token(ascii, ''') <~ constantString1("'")) ~ (token(ascii, ''') <~ constantString1("'"))
    }

    def parameter(bits: BitVector, cName: Option[String], cCharset: Option[String]): Attempt[DecodeResult[(String, String, Option[Int], Option[String])]] = {
      attributeWithExtension.decode(bits).flatMap { result =>
        val (name ,(index, encoded)) = result.value
        if (cName.contains(name)) {
          decodeValue(result.remainder, cCharset).map(_.map(v => (name, v, index, cCharset)))
        } else {
          if (encoded) {
            charsetAndLang.decode(result.remainder).flatMap { res =>
              decodeValue(res.remainder, Some(res.value._1).filter(_.nonEmpty) orElse Some("utf-8")).map(_.map(v => (name, v, index, Some(res.value._1))))
            }
          } else {
            decodeValue(result.remainder, None).map(_.map(v => (name, v, index, cCharset)))
          }

        }
      }
    }

    def urlEncodedValue(charset: String): Codec[String] = {
      takeWhileChar(ascii)(';').exmap(encoded => attempt(URLDecoder.decode(encoded, charset)), decoded => attempt(URLEncoder.encode(decoded, charset)))
    }

    def decodeValue(bits: BitVector, charset: Option[String]): Attempt[DecodeResult[String]] = {
      quotedAsciiString.decode(bits).flatMap { result =>
        attempt(URLDecoder.decode(result.value, charset.getOrElse("utf-8"))).map { decoded =>
          DecodeResult(decoded, result.remainder)
        }
      } orElse
        charset.map(urlEncodedValue(_).decode(bits)).getOrElse(takeWhileChar(ascii)(';').decode(bits))
    }


    def decodeParameters(bits: BitVector, result: Vector[(String, String)]): Attempt[DecodeResult[Vector[(String, String)]]] = {
      if (bits.isEmpty) Attempt.successful(DecodeResult(result, BitVector.empty))
      else parameter(bits, None, None).flatMap { res =>
        val (name, value, index, charset) = res.value
        index match {
          case None =>
            decodeParameters(res.remainder, (name -> value) +: result)

          case Some(idx) =>
            decodeContinuation(res.remainder, name, value, idx, charset, result)
        }
      }
    }

    def decodeContinuation(bits: BitVector, cName: String, cValue: String, cIdx: Int, cCharset: Option[String], result: Vector[(String, String)]): Attempt[DecodeResult[Vector[(String, String)]]] = {
      if (bits.isEmpty) Attempt.successful(DecodeResult((cName -> cValue) +: result, BitVector.empty))
      else parameter(bits, Some(cName), cCharset).flatMap { res =>
        val (name, value, index, charset) = res.value
        index match {
          case None =>
            decodeParameters(res.remainder, Vector(cName -> cValue, name -> value) ++ result)
          case Some(idx) =>
            if (name == cName && idx > cIdx) {
              decodeContinuation(res.remainder, cName, cValue + value, idx, charset, result)
            } else  {
              decodeContinuation(res.remainder, name, value, idx, charset, (cName -> cValue) +: result)
            }
        }
      }
    }


    def encodeValue(name: String, value: String, charset: String): Attempt[String] = {

      lazy val encoder = Charset.forName(charset).newEncoder()

      def go(remains: String, buff: String, acc: Seq[String], highSurrogate: Option[Char]): Attempt[String] = {
        remains.headOption match {
          case Some(ch) =>
            if (ch.isHighSurrogate) go(remains.tail, buff, acc, Some(ch))
            else {
              val encodedChar = ByteVector.view(encoder.encode(CharBuffer.wrap(highSurrogate.toArray ++ Array(ch)))).toHex.toUpperCase.grouped(2).flatMap("%" + _).mkString
              if (buff.isEmpty) go(remains.tail, s"$name*0*=$charset''$encodedChar", acc, None)
              else if (buff.length + encodedChar.length > maxValueLength) {
                val newLine = s"$name*${acc.size + 1}*=$encodedChar"
                go(remains.tail, newLine, acc :+ buff, None)
              } else {
                go(remains.tail, buff + encodedChar, acc, None)
              }
            }

          case None =>
            val lines = (acc :+ buff).filter(_.nonEmpty)
            val result = lines.mkString(";\r\n ", ";\r\n ", "")
            if (lines.size == 1) Attempt.successful(result.replaceFirst("\\*0", ""))
            else Attempt.successful(result)
        }
      }

      if (value.forall(AsciiEncoder.canEncode)) {
        val param = if (value.exists(defaultQuotableChars.contains)) {
          s""";\r\n $name="$value""""
        } else {
          s";\r\n $name=$value"
        }

        if (param.length < maxValueLength) Attempt.successful(param)
        else go(value, "", Seq.empty, None)
      } else {
        go(value, "", Seq.empty, None)
      }

    }

    def encodeRFC2184(params: Vector[(String, String)]): Attempt[BitVector] = {
      params.foldRight(Attempt.successful(Vector.empty[String])) { case ((name, value), result) =>
        result.flatMap { r => encodeValue(name, value, "utf-8").map(_ +: r) }
      }.map(_.mkString) flatMap ascii.encode
    }
  }

}
