package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.HeaderCodecDefinition
import spinoco.protocol.mime.ContentDisposition


/**
  *   RFC 7578, RFC 6266, RFC 2183
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Disposition
  */
sealed case class `Content-Disposition`(value: ContentDisposition) extends DefaultHeader with ContentHeaderField


object `Content-Disposition` { val codec =
  HeaderCodecDefinition.contentField[ `Content-Disposition`](ContentDisposition.htmlCodec.xmap (cd => `Content-Disposition`(cd), _.value))
}
