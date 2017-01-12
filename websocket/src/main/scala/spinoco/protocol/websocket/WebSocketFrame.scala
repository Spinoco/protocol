package spinoco.protocol.websocket

import scodec.bits.{BitVector, ByteVector}

/**
  * RFC 6455 Websocket frame
  *
  * @param fin        If true, this is final Frame
  * @param rsv        RSV._1 to RSV._3 bits
  * @param opcode     OpCode of this frame
  * @param payload    Payload data
  * @param mask       Masking bits for the payload (32 bits)
  */
case class WebSocketFrame(
  fin: Boolean
  , rsv: (Boolean, Boolean, Boolean)
  , opcode: OpCode.Value
  , payload: ByteVector
  , mask: Option[BitVector]
)


object OpCode extends Enumeration {

  val Continuation = Value(0)
  val Text = Value(1)
  val Binary = Value(2)
  val Close = Value(8)
  val Ping = Value(9)
  val Pong = Value(0xA)


}