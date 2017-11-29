package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition



/**
  *   RFC 7231 3.3.1
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Transfer-Encoding
  */
sealed case class `Transfer-Encoding`(value: List[String]) extends DefaultHeader

object `Transfer-Encoding` { val codec =
  HeaderCodecDefinition[`Transfer-Encoding`](commaDelimitedMin(trimmedAsciiToken, 1).xmap (`Transfer-Encoding`.apply,_.value))
}
