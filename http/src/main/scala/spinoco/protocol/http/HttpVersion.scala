package spinoco.protocol.http

import scodec.{Attempt, Codec}
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.common.util._


object HttpVersion extends Enumeration {

  val V1_0 = Value("1.0")
  val V1_1 = Value("1.1")

  val codec: Codec[HttpVersion.Value] = {
    utf8String.exmap(
      s => attempt(HttpVersion.withName(s.drop(5)))
      , v => Attempt.successful(s"HTTP/$v")
    )
  }

}
