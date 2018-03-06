package spinoco.protocol.mime

import scodec._
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.mime.MediaType.{CustomMediaType, DefaultMediaType, MultipartMediaType}

sealed trait ContentType {
  def mediaType: MediaType
}

object ContentType {

  case class TextContent(mediaType: DefaultMediaType, charset: Option[MIMECharset]) extends ContentType
  case class BinaryContent(mediaType: DefaultMediaType, charset: Option[MIMECharset]) extends ContentType
  case class MultiPartContent(mediaType: MultipartMediaType, charset: Option[MIMECharset]) extends ContentType
  case class CustomContent(mediaType: CustomMediaType) extends ContentType

  case class Parameters(charset: Option[MIMECharset], others: Map[String, String])

  val codec: Codec[ContentType] = {

    val semicolon = Codec(constantString1("; "), constantString1(";"))

    val charset: Codec[(String, Either[MIMECharset, String])] = {
      (semicolon ~> ignoreWS ~> constantString1CaseInsensitive("charset=") ~> MIMECharset.separatedBy(';') <~ ignoreWS).widen("charset" -> Left(_), { case (param, value) =>
        value match {
          case Left(mimeCharset) if param == "charset" => Attempt.successful(mimeCharset)
          case _ =>  Attempt.failure(Err(s"Failed to encode Multipart parameter. Expected charset, got $param. For value: $value"))

        }
      })
    }

    val mediaTypeCodec: Codec[MediaType] =
      token(ascii, ';').exmap(MediaType.decodeString, MediaType.encodeString)

    def parameter(tokenCodec: Codec[String]): Codec[(String, Either[MIMECharset, String])] = {
      (semicolon ~> ignoreWS ~> (takeWhile(utf8)(_ != '='.toByte) <~ constantString1("=")) ~ tokenCodec <~ ignoreWS).widen({ case (name, value) => name.toLowerCase -> Right(value)}, { case (param, value) =>
        value match {
          case Right(strValue) => Attempt.successful(param -> strValue)
          case _ => Attempt.failure(Err(s"Failed to encode Multipart parameter. Expected String value, got MIMECharset"))
        }
      })
    }

    def readParameters(mediaType: MediaType, required: Seq[String], tokenCodec: Codec[String] = choice(quotedAsciiToken, asciiToken)): Codec[Parameters] = {
      list(choice(charset, parameter(tokenCodec))).exmap[Parameters](
        { parameters =>
          val names = parameters.map(_._1).toSet
          if (required.forall(names.contains)) {
            Attempt.successful(
              Parameters(parameters.collectFirst { case ("charset", Left(mimeCharset)) => mimeCharset }, parameters.collect { case (name, Right(value)) => name -> value } toMap)
            )
          } else {
            Attempt.failure(Err(s"Invalid media type. Parameters ${required.mkString(", ")} are required for $mediaType, got: $parameters"))
          }
        }
        , { case Parameters(charset, params) =>

          if (required.forall(params.isDefinedAt)) {
            Attempt.successful(params.mapValues(Right(_)).toList ++ charset.map("charset" -> Left(_)).toList)
          } else {
            Attempt.failure(Err(s"Invalid media type. Parameters ${required.mkString(", ")} are required for $mediaType"))
          }
        }
      )
    }

    def contentTypeParameters(mediaType: MediaType): Codec[Parameters] = {
      mediaType match {
        case MultipartMediaType("signed", _) => readParameters(mediaType, Seq("protocol", "micalg", "boundary"))
        case MultipartMediaType("encrypted", _) => readParameters(mediaType, Seq("protocol", "boundary"))
        case MultipartMediaType(_, _) => readParameters(mediaType, Seq("boundary"))
        case _ =>  readParameters(mediaType, Seq(), asciiToken)
      }
    }

    mediaTypeCodec.flatZip[Parameters](contentTypeParameters).narrow[ContentType](
      {
        case (media: MultipartMediaType, Parameters(charset, parameters)) => Attempt.successful(MultiPartContent(media.copy(parameters = parameters), charset))
        case (media: DefaultMediaType, Parameters(charset, _)) => Attempt.successful(if (media.isText) TextContent(media, charset) else BinaryContent(media, charset))
        case (media: CustomMediaType, _) => Attempt.successful(CustomContent(media))
      }, {
        case TextContent(media, charset) => (media, Parameters(charset, Map()))
        case BinaryContent(media, charset) => (media, Parameters(charset, Map()))
        case MultiPartContent(media, charset) => (media, Parameters(charset, media.parameters))
        case CustomContent(media) => (media, Parameters(None, Map()))
      }
    )
  }

}
