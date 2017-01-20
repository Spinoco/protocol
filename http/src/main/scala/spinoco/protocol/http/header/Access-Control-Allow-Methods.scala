package spinoco.protocol.http.header

import spinoco.protocol.http.HttpMethod
import spinoco.protocol.http.header.value.HeaderCodecDefinition
import spinoco.protocol.http.codec.helper.commaDelimitedMin

/**
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Methods
  */
sealed case class `Access-Control-Allow-Methods`(value: List[HttpMethod.Value]) extends DefaultHeader


object `Access-Control-Allow-Methods` { val codec =
  HeaderCodecDefinition[`Access-Control-Allow-Methods`](commaDelimitedMin(HttpMethod.codec, 1).xmap (`Access-Control-Allow-Methods`.apply, _.value))
}