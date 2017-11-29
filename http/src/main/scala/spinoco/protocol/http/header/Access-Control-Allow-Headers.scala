package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Headers
  */
sealed case class `Access-Control-Allow-Headers`(value: List[String]) extends DefaultHeader


object `Access-Control-Allow-Headers` { val codec =
  HeaderCodecDefinition[`Access-Control-Allow-Headers`](commaDelimitedMin(trimmedAsciiToken, 1).xmap (`Access-Control-Allow-Headers`.apply, _.value))
}