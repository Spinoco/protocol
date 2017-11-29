package spinoco.protocol.mgcp.codec

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.mgcp.{MGCPResponse, MGCPResponseCode}
import spinoco.protocol.sdp.codec.SessionDescriptionCodec

/**
  * Created by pach on 09/03/17.
  */
object MGCPResponseCodec {

  lazy val codec: Codec[MGCPResponse] = "MGCP Response" | {
    import impl._
    (
      header("Header" | (
          ("Command Line" | untilEOL(
          ("Response Code" | untilWs(responseCode)) :: WS ::
          ("Tx Id" | untilWs(transactionId)) :: WS ::
          ("Package Name" | optional(recover2(constantString1("/")), untilWs(utf8) <~ WS)) ::
          ("Response String" |  optional(bitsRemaining, utf8))
          )) :+
          ("MGCP Paramater" | list(MGCPParameterCodec.codec))
      )) :+
        ("SDP" | (optional(bitsRemaining, choice(`\r\n`, `\n`) ~> SessionDescriptionCodec.codec)))
    ).as[MGCPResponse]

  }

  object impl {

    val responseCode: Codec[MGCPResponseCode] = {
      intAsString.xmap(
        {
          case i if i < 100 || (i >= 200 && i < 300) => MGCPResponseCode.Success(i)
          case i if i >=100 && i < 200 => MGCPResponseCode.Provisional(i)
          case i if i < 800 => MGCPResponseCode.Error(i)
          case i => MGCPResponseCode.PackageSpecific(i)
        }
        , _.code
      )
    }

  }

}
