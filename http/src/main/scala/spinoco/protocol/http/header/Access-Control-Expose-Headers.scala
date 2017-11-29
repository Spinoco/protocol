package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *   @see  https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Expose-Headers
  */
sealed case class `Access-Control-Expose-Headers`(value: List[String]) extends DefaultHeader

object `Access-Control-Expose-Headers` {val codec =
  HeaderCodecDefinition[`Access-Control-Expose-Headers`](commaDelimitedMin(trimmedAsciiToken, 1).xmap (`Access-Control-Expose-Headers`.apply, _.value))
}

