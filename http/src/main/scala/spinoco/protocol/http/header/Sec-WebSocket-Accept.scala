package spinoco.protocol.http.header

import scodec.bits.ByteVector
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  * Confirmation of the server to establish a websocket connection.
  * @param confirm  SHA1 of concatenation of the (key (as string) with "258EAFA5-E914-47DA-95CA-C5AB0DC85B11)
  */
case class `Sec-WebSocket-Accept`(confirm: ByteVector) extends DefaultHeader


object `Sec-WebSocket-Accept` { val codec =
  HeaderCodecDefinition[`Sec-WebSocket-Accept`](base64Encoded.xmap (`Sec-WebSocket-Accept`.apply,_.confirm))
}