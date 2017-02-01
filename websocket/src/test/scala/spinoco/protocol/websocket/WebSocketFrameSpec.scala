package spinoco.protocol.websocket

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._
import scodec.Attempt
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.websocket.codec.WebSocketFrameCodec


object WebSocketFrameSpec extends Properties("WebSocketFrame") {

  def decode(data: ByteVector):Attempt[WebSocketFrame] =
    WebSocketFrameCodec.codec.decodeValue(data.bits)

  def decode(hex: String):Attempt[WebSocketFrame] =
    decode( ByteVector.fromHex(hex).get)

  def encode(frame: WebSocketFrame):Attempt[BitVector] =
    WebSocketFrameCodec.codec.encode(frame)

  def decodeAndEncode(hex: String)(expect: WebSocketFrame): Prop = {
    val data = ByteVector.fromHex(hex).get
    decode(data).flatMap { wsf =>
    encode(wsf).map { bits =>
      "Decode" |: (wsf ?= expect) && ("Encode" |: (bits.bytes ?= data) )
    }}.fold(err => err.toString() |: falsified, identity)
  }

  property("single-frame-unmasked") = secure {
    decodeAndEncode("810548656c6c6f")(
      WebSocketFrame(
        fin = true
        , rsv = (false, false, false)
        , opcode = OpCode.Text
        , payload = ByteVector("Hello".getBytes)
        , mask = None
      )
    )
  }

  property("single-frame-masked") = secure {
    decodeAndEncode("818537fa213d7f9f4d5158")(
      WebSocketFrame(
        fin = true
        , rsv = (false, false, false)
        , opcode = OpCode.Text
        , payload = ByteVector("Hello".getBytes)
        , mask = Some(939139389)
      )
    )
  }

  property("fragmented-frame-unmasked") = secure {
    decodeAndEncode("010348656c")(
      WebSocketFrame(
        fin = false
        , rsv = (false, false, false)
        , opcode = OpCode.Text
        , payload = ByteVector("Hel".getBytes)
        , mask = None
      )
    ) && decodeAndEncode("80026c6f")(
      WebSocketFrame(
        fin = true
        , rsv = (false, false, false)
        , opcode = OpCode.Continuation
        , payload = ByteVector("lo".getBytes)
        , mask = None
      )
    )
  }

  property("unmasked-ping") = secure {
    decodeAndEncode("890548656c6c6f")(
      WebSocketFrame(
        fin = true
        , rsv = (false, false, false)
        , opcode = OpCode.Ping
        , payload = ByteVector("Hello".getBytes)
        , mask = None
      )
    )
  }

  property("masked-pong") = secure {
    decodeAndEncode("8a8537fa213d7f9f4d5158")(
      WebSocketFrame(
        fin = true
        , rsv = (false, false, false)
        , opcode = OpCode.Pong
        , payload = ByteVector("Hello".getBytes)
        , mask = Some(939139389)
      )
    )
  }

  property("binary-256-bytes") = secure {
    decodeAndEncode("827e00000100" + "aa"*256)(
      WebSocketFrame(
        fin = true
        , rsv = (false, false, false)
        , opcode = OpCode.Binary
        , payload = ByteVector.fromHex("aa"*256).get
        , mask = None
      )
    )
  }


  property("binary-65536-bytes") = secure {
    decodeAndEncode("827e00010000" + "aa"*65536)(
      WebSocketFrame(
        fin = true
        , rsv = (false, false, false)
        , opcode = OpCode.Binary
        , payload = ByteVector.fromHex("aa"*65536).get
        , mask = None
      )
    )
  }



}
