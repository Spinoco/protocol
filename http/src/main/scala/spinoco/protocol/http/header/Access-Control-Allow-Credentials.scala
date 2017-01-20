package spinoco.protocol.http.header

import spinoco.protocol.common.codec._
import spinoco.protocol.http.header.value.HeaderCodecDefinition


/**
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Credentials
  */
sealed case class `Access-Control-Allow-Credentials`(value: Boolean) extends DefaultHeader


object `Access-Control-Allow-Credentials` { val codec =
  HeaderCodecDefinition[`Access-Control-Allow-Credentials`](boolAsString.xmap (`Access-Control-Allow-Credentials`.apply, _.value))
}