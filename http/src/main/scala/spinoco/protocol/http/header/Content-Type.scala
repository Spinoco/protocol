package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.HeaderCodecDefinition
import spinoco.protocol.mime.ContentType



/**
  *   RFC 7231 section 3.1.1.5
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Type
  */
sealed case class `Content-Type`(value: ContentType) extends DefaultHeader with ContentHeaderField


object `Content-Type` {
  val codec = HeaderCodecDefinition.contentField[`Content-Type`](ContentType.codec.xmap (`Content-Type`.apply, _.value))
}
