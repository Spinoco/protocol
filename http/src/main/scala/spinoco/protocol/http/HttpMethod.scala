package spinoco.protocol.http

import scodec.Codec
import codec.helper._
import spinoco.protocol.common.util._


object HttpMethod extends Enumeration {

  val CONNECT = Value
  val DELETE  = Value
  val GET     = Value
  val HEAD    = Value
  val OPTIONS = Value
  val PATCH   = Value
  val POST    = Value
  val PUT     = Value
  val TRACE   = Value


  val codec: Codec[HttpMethod.Value] = {
    import scodec.Attempt._
    trimmedAsciiToken.exmap(
      s => attempt(HttpMethod.withName(s))
      , m => successful(m.toString)
    )
  }



}
