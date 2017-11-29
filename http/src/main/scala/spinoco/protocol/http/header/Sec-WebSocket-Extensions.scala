package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  * Indications of websocket protocol extensions that client/server wants to speak.
  *
  * @param extensions List of extensions
  */
case class `Sec-WebSocket-Extensions`(extensions: List[String]) extends DefaultHeader


object `Sec-WebSocket-Extensions` { val codec =
  HeaderCodecDefinition[`Sec-WebSocket-Extensions`](commaDelimitedMin(trimmedAsciiToken, 1).xmap (`Sec-WebSocket-Extensions`.apply,_.extensions))
}
