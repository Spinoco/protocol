package spinoco.protocol.http.codec

import scodec.{Codec, codecs}
import spinoco.protocol.http.codec.helper.crlf
import spinoco.protocol.http.header.ContentHeaderField
import spinoco.protocol.http.mime.MIMEHeader

object HttpMimeHeaderCodec {

  val defaultCodec: Codec[MIMEHeader] =
    codec(HttpHeaderCodec.contentCodec(Int.MaxValue))

  def codec(
    headerCodec: Codec[ContentHeaderField]
  ): Codec[MIMEHeader] = {
    codecs.listDelimited[ContentHeaderField](crlf.bits, headerCodec).xmap[MIMEHeader](
      headers => MIMEHeader(headers)
      , header => header.fields
    )
  }

}
