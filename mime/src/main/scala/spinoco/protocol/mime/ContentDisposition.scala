package spinoco.protocol.mime

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.common.Terminator

sealed case class ContentDisposition(dispositionType: String, parameters: Map[String, String] )


object ContentDisposition {

  val codec: Codec[ContentDisposition] = {

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


    ((ignoreWS ~> token(ascii, ';')) :: mapCodec)
    .as[ContentDisposition]

  }

}