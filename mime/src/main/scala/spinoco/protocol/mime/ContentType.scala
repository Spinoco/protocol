package spinoco.protocol.mime

import scodec._
import scodec.codecs._
import shapeless.HNil
import shapeless.::
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
      ignoreWS ~> constantString1("charset=") ~> MIMECharset.codec <~ ignoreWS
    }
    val boundary: Codec[String] = {
      ignoreWS ~> constantString1("boundary=") ~> asciiToken <~ ignoreWS
    }

    val quotedBoundary: Codec[String] = {
      semicolon ~> ignoreWS ~> constantString1("boundary=") ~> quotedAsciiToken <~ ignoreWS
    }

    val quotedProtocol: Codec[String] = {
      semicolon ~> ignoreWS ~> constantString1("protocol=") ~> quotedAsciiToken <~ ignoreWS
    }

    val micalg: Codec[String] = {
      semicolon ~> ignoreWS ~> constantString1("micalg=") ~> quotedAsciiToken <~ ignoreWS
    }

    val mediaTypeCodec: Codec[MediaType] =
      token(ascii, ';').exmap(MediaType.decodeString, MediaType.encodeString)

    def signedParameters(mediaType: MediaType): Codec[Parameters] = {
      (quotedProtocol :: micalg :: quotedBoundary).widen[Parameters](
        {
          case p :: m :: b :: HNil => Parameters(None, Map("protocol" -> p, "micalg" -> m, "boundary" -> b))
        }, { case Parameters(_, params) =>
          val parameters = for {
            p <- params.get("protocol")
            m <- params.get("micalg")
            b <- params.get("boundary")
          } yield p :: m :: b :: HNil

          parameters match {
            case Some(required) => Attempt.successful(required)
            case None => Attempt.failure(Err(s"Invalid media type. Parameters protocol, micalg and boundary are required for multipart/signed media type : $mediaType"))
          }
        }
      )
    }

    def encryptedParameters(mediaType: MediaType): Codec[Parameters] = {
      (quotedProtocol :: quotedBoundary).widen[Parameters](
        {
          case p :: b :: HNil => Parameters(None, Map("protocol" -> p, "boundary" -> b))
        }, { case Parameters(_, params) =>
          val parameters = for {
            p <- params.get("protocol")
            b <- params.get("boundary")
          } yield p :: b :: HNil

          parameters match {
            case Some(required) => Attempt.successful(required)
            case None => Attempt.failure(Err(s"Invalid media type. Parameters protocol and boundary are required for multipart/encrypted media type : $mediaType"))
          }
        }
      )
    }

    def multipartParameters(mediaType: MediaType): Codec[Parameters] = {
      (semicolon ~> boundary).widen[Parameters](
        boundary => Parameters(None, Map("boundary" -> boundary))
        , _.others.get("boundary").fold[Attempt[String]](
          Attempt.failure(Err(s"Invalid media type. Parameter boundary is required for multipart media type : $mediaType"))
        )(Attempt.successful)
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
        case MultipartMediaType("signed", _) => signedParameters(mediaType)
        case MultipartMediaType("encrypted", _) => encryptedParameters(mediaType)
        case MultipartMediaType(_, _) => multipartParameters(mediaType)
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
