package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpOrigin}

/**
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin
  */
sealed case class `Access-Control-Allow-Origin`(value: HttpOrigin) extends DefaultHeader

object `Access-Control-Allow-Origin` { val codec =
  HeaderCodecDefinition[`Access-Control-Allow-Origin`](HttpOrigin.codec.xmap (`Access-Control-Allow-Origin`.apply, _.value))
}

