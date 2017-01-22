package spinoco.protocol.http.header

import scodec.bits.ByteVector
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  * @param key 16 byte key used when handshaking websocket connection.
  */
case class `Sec-WebSocket-Key`(key: ByteVector) extends DefaultHeader


object `Sec-WebSocket-Key` { val codec =
  HeaderCodecDefinition[`Sec-WebSocket-Key`](base64Encoded.xmap (`Sec-WebSocket-Key`.apply,_.key))
}