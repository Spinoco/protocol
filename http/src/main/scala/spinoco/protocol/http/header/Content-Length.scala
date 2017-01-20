package spinoco.protocol.http.header

import spinoco.protocol.common.codec._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *   RFC 7230 section 3.3.2
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Length
  */
sealed case class `Content-Length`(value: Long) extends DefaultHeader


object `Content-Length` { val codec =
  HeaderCodecDefinition[`Content-Length`](longAsString.xmap (`Content-Length`.apply, _.value))
}