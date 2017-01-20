package spinoco.protocol.http.header.value

import scodec.Codec
import spinoco.protocol.http.codec.helper._


sealed case class HttpEncoding (value: String)

object HttpEncoding {

  val codec: Codec[HttpEncoding] = {
    trimmedAsciiString.xmap(HttpEncoding.apply, _.value)
  }
}