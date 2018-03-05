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
  case class MultiPartContent(mediaType: MultipartMediaType) extends ContentType
  case class CustomContent(mediaType: CustomMediaType) extends ContentType

  case class Parameters(charset: Option[MIMECharset], others: Map[String, String])

  val codec: Codec[ContentType] = {

    val semicolon = Codec(constantString1("; "), constantString1(";"))

    val charset: Codec[MIMECharset] = {
      ignoreWS ~> constantString1CaseInsensitive("charset=") ~> MIMECharset.codec <~ ignoreWS
    }
    val boundary: Codec[String] = {
      ignoreWS ~> constantString1CaseInsensitive("boundary=") ~> asciiToken <~ ignoreWS
    }

    val mediaTypeCodec: Codec[MediaType] =
      token(ascii, ';').exmap(MediaType.decodeString, MediaType.encodeString)

    def parameter(name: String, tokenCodec: Codec[String]): Codec[(String, String)] = {
      (semicolon ~> ignoreWS ~> constantString1CaseInsensitive(s"$name=") ~> tokenCodec <~ ignoreWS).widen(name -> _, { case (param, value) =>
          if (param == name) Attempt.successful(value)
          else Attempt.failure(Err(s"Failed to encode Multipart parameter. Expected $name, got $param"))
      })
    }

    def multipartParameters(mediaType: MediaType, required: Seq[String], tokenCodec: Codec[String] = choice(quotedAsciiToken, asciiToken)): Codec[Parameters] = {
      list(choice(required.map(parameter(_, tokenCodec)): _*)).widen[Parameters](
        { parameters => Parameters(None, parameters.toMap) }
        , { case Parameters(_, params) =>
          val requiredParameters = for {
            name <- required
            param <- params.get(name)
          }  yield name -> param

          if (requiredParameters.size == required.size) {
            Attempt.successful(requiredParameters.toList)
          } else {
            Attempt.failure(Err(s"Invalid media type. Parameters ${required.mkString(", ")} are required for $mediaType"))
          }
        }
      )
    }

    val defaultParameters: Codec[Parameters] = {
      optional(
        lookahead2(constantString1(";"))
        , semicolon ~> choice(
          charset.xmap[Right[Nothing, MIMECharset]](Right(_), { case Right(r) => r }).upcast[Either[String, MIMECharset]]
          , boundary.xmap[Left[String, Nothing]](Left(_), { case Left(l) => l }).upcast[Either[String, MIMECharset]]
        )
      ).xmap[Parameters](
        { _.fold(Parameters(None, Map())) {
            case Right(ch) => Parameters(Some(ch), Map())
            case Left(b) => Parameters(None, Map("boundary" -> b))
          }
        }, { case Parameters(charset, params) => params.get("boundary").map(Left(_)) orElse charset.map(Right(_)) }
      )
    }

    def contentTypeParameters(mediaType: MediaType): Codec[Parameters] = {
      mediaType match {
        case MultipartMediaType("signed", _) => multipartParameters(mediaType, Seq("protocol", "micalg", "boundary"))
        case MultipartMediaType("encrypted", _) => multipartParameters(mediaType, Seq("protocol", "boundary"))
        case MultipartMediaType(_, _) => multipartParameters(mediaType, Seq("boundary"), asciiToken)
        case _ =>  defaultParameters
      }
    }

    mediaTypeCodec.flatZip[Parameters](contentTypeParameters).narrow[ContentType](
      {
        case (media: MultipartMediaType, Parameters(_, parameters)) => Attempt.successful(MultiPartContent(media.copy(parameters = parameters)))
        case (media: DefaultMediaType, Parameters(charset, _)) => Attempt.successful(if (media.isText) TextContent(media, charset) else BinaryContent(media, charset))
        case (media: CustomMediaType, _) => Attempt.successful(CustomContent(media))
      }, {
        case TextContent(media, charset) => (media, Parameters(charset, Map()))
        case BinaryContent(media, charset) => (media, Parameters(charset, Map()))
        case MultiPartContent(media) => (media, Parameters(None, media.parameters))
        case CustomContent(media) => (media, Parameters(None, Map()))
      }
    )
  }

}
