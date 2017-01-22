package spinoco.protocol.http.header


import spinoco.protocol.http.header.value.HeaderCodecDefinition
import spinoco.protocol.common.codec._

/**
  * A Websocket header indicating websocket protocol version.
  *
  * @param version Version value
  */
case class `Sec-WebSocket-Version`(version: Int) extends DefaultHeader


object `Sec-WebSocket-Version` { val codec =
  HeaderCodecDefinition[`Sec-WebSocket-Version`](intAsString.xmap (`Sec-WebSocket-Version`.apply,_.version))
}
