package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  * Header indicating protocols the client wishes to speak to server
  *
  * @param protocols  Protocols in order of the preference
  */
case class `Sec-WebSocket-Protocol`(protocols: List[String]) extends DefaultHeader


object `Sec-WebSocket-Protocol` { val codec =
  HeaderCodecDefinition[`Sec-WebSocket-Protocol`](commaDelimitedMin(trimmedAsciiToken, 1).xmap (`Sec-WebSocket-Protocol`.apply,_.protocols))
}