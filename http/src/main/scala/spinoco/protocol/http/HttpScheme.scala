package spinoco.protocol.http

import scodec.Codec
import codec.helper._
import spinoco.protocol.common.util._


object HttpScheme extends Enumeration {

  val HTTP = Value("http")

  val HTTPS = Value("https")

  val WS = Value("ws")

  val WSS = Value("wss")


  val codec: Codec[HttpScheme.Value] = {
    import scodec.Attempt._
    trimmedAsciiToken.exmap(
      s => attempt(HttpScheme.withName(s.toLowerCase))
      , s => successful(s.toString)
    )
  }

}
