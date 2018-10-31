package spinoco.protocol.mime

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.mime.codec.RFC2184Codec

sealed case class ContentDisposition(
  dispositionType: ContentDispositionType, parameters: Map[String, String] )

sealed trait ContentDispositionType

/**
  * Content-Disposition type as described at https://www.ietf.org/rfc/rfc2183.txt
  */
object ContentDispositionType {
  object Attachment extends ContentDispositionType
  object Inline extends ContentDispositionType
  case class XToken(token: String) extends ContentDispositionType
  case class IETFToken(token: String) extends ContentDispositionType
}

object ContentDisposition {

  val dispositionTypeCodec: Codec[ContentDispositionType] = {
    import ContentDispositionType._
    ignoreWS ~>
      token(ascii, ';').xmap({
        case "attachment" => Attachment
        case "inline" => Inline
        case value =>
          if (value.take(2).toLowerCase == "x-") XToken(value.drop(2))
          else IETFToken(value)
      }, {
        case XToken(token) => s"X-$token"
        case IETFToken(token) => token
        case Attachment => "attachment"
        case Inline => "inline"
      }
    )
  }

  val codec: Codec[ContentDisposition] = {

    val mapCodec:Codec[Map[String, String]] =
      optional[Vector[(String, String)]](
        lookahead2(constantString1(";"))
        , RFC2184Codec.codec
      ).xmap[Map[String, String]](
        { o => o.map(_.toMap).getOrElse(Map.empty) }
        , { m => m.headOption.map { _ => m.toVector } }
      )

    (dispositionTypeCodec :: mapCodec).as[ContentDisposition]

  }

}