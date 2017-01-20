package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *
  *   @see  https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Request-Headers
  */
sealed case class `Access-Control-Request-Headers`(value: List[String]) extends DefaultHeader



object `Access-Control-Request-Headers` { val codec =
  HeaderCodecDefinition[`Access-Control-Request-Headers`](commaDelimitedMin(trimmedAsciiString, 1).xmap (`Access-Control-Request-Headers`.apply, _.value))
}