package spinoco.protocol.websocket

import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Prop._
import scodec.{Attempt, DecodeResult, Err}
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

  private val payloadLengths = {
    val specialLengths = Seq(124, 125, 126, 127, 128, 256, 257, 65535, 65536, Int.MaxValue)

    Gen.chooseNum(0, Long.MaxValue, specialLengths.map(_.toLong): _*)
  }

  property("binary-bytes") = forAll(payloadLengths.filter(_ < BigInt(2).pow(17))) { (length: Long) =>
    (length >= 0) ==> {

      val prefix = {
        def lengthToHex(numChars: Int) = s"%${numChars}x".format(length).replace(" ", "0")

        if (length <= 125)        "82" + lengthToHex(2)
        else if (length <= 65535) "827e" + lengthToHex(4)
        else                      "827f" + lengthToHex(16)
      }

      val data = "aa"*length.toInt

      decodeAndEncode(prefix + data)(
        WebSocketFrame(
          fin = true
          , rsv = (false, false, false)
          , opcode = OpCode.Binary
          , payload = ByteVector.fromHex(data).get
          , mask = None
        )
      )
    }
  }

  property("payload-length") = forAll(payloadLengths) { (length: Long) =>
    (length >= 0) ==> {

      val codec = WebSocketFrameCodec.impl.payloadLength

      val bits = {
        if (length <= 125)        BitVector.fromInt(length.toInt, 7)
        else if (length <= 65535) BitVector.fromInt(126, 7) ++ BitVector.fromInt(length.toInt, 16)
        else                      BitVector.fromInt(127, 7) ++ BitVector.fromLong(length)
      }

      if (length <= Int.MaxValue) {
        val decodedLength = codec.decode(bits)
        val encodedBits = codec.encode(length.toInt)

        val decode = "Decode" |: (decodedLength ?= Attempt.successful(DecodeResult(length.toInt, BitVector.empty)))
        val encode = "Encode" |: (encodedBits ?= Attempt.successful(bits))
        decode && encode
      } else {
        codec.decodeValue(bits) ?= Attempt.failure(Err(s"Max supported size is ${Int.MaxValue}, got $length"))
      }

    }
  }

}
