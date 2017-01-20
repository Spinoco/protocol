package spinoco.protocol.http.header.value

import scodec.Codec
import spinoco.protocol.http.codec.helper._

sealed case class ContentDisposition(dispositionType: String, parameters: Map[String, String] )


object ContentDisposition {

  val codec:Codec[ContentDisposition] = {

    val paramsCodec:Codec[(String,String)] = {
      tuple[String,String](_equal, trimmedAsciiString, quotedString)
    }

    parametrizedN(semicolon, semicolon_SP, trimmedAsciiString, paramsCodec).xmap(
      { case (tpe, params) => ContentDisposition(tpe, params.toMap)  }
      , cd => cd.dispositionType -> cd.parameters.toList
    )

  }

}