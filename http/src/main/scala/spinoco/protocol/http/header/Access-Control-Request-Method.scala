package spinoco.protocol.http.header

import spinoco.protocol.http.HttpMethod
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Request-Method
  */
sealed case class `Access-Control-Request-Method`(value: HttpMethod.Value) extends DefaultHeader

object `Access-Control-Request-Method` { val codec =
  HeaderCodecDefinition[`Access-Control-Request-Method`](HttpMethod.codec.xmap (`Access-Control-Request-Method`(_), _.value))
}