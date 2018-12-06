package spinoco.protocol.mime

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.common.Terminator
import spinoco.protocol.mime.codec.RFC2184Codec

sealed case class ContentDisposition(dispositionType: ContentDispositionType, parameters: Map[String, String])

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

  val emailCodec: Codec[ContentDisposition] = {

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

  val htmlCodec: Codec[ContentDisposition] = {

    val paramsCodec: Codec[(String, String)] =    {
      ignoreWS ~> terminated(asciiToken, Terminator.constantString1("=")) ~
        (ignoreWS ~> eventuallyQuotedUTF8String <~ ignoreWS)
    }

    val semicolon = Codec(constantString1("; "), constantString1(";"))

    val mapCodec:Codec[Map[String, String]] =
      optional[Map[String, String]](
        lookahead2(constantString1(";"))
        , semicolon ~> vectorVDelimited(paramsCodec, semicolon).xmap[Map[String, String]](_.toMap, _.toVector)
      ).xmap[Map[String, String]](
      { o => o.getOrElse(Map.empty) }
      , { m => m.headOption.map { _ => m } }
      )


    (dispositionTypeCodec :: mapCodec).as[ContentDisposition]
  }

}