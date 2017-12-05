package spinoco.protocol.rtp.codec

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._
import scodec.{Attempt, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.rtp.{RTPHeaderExtension, RTPPacket}


object RtpPacketCodecSpec extends Properties("RtpPacketCodec") {

  def encodeDecode(packet: RTPPacket, hexBytes: String) : Prop = {
    val rtpData = BitVector.fromHex(hexBytes).get
    (s"Bytes are aligned : $rtpData" |: (rtpData.bytes.size % 4 ?= 0)) &&
    ( "Encoded" |: (RTPPacketCodec.codec.encode(packet) ?= Attempt.successful(rtpData))) &&
    ( "Decoded" |: (RTPPacketCodec.codec.decode(rtpData) ?= Attempt.successful(DecodeResult(packet, BitVector.empty))))
  }

  property("decode-encode.standard.v2") = protect {
    val rtpData = BitVector.fromHex("8008ea1a25a906ad56020720ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").get
    val packet = RTPPacket(
      version = RTPVersion.V_2
      , marker = false
      , payloadType = 8
      , sequenceNumber = 59930
      , timestamp = 631834285
      , ssrc = 1442973472
      , csrc = Nil
      , payload = rtpData.bytes.drop(4*3) // drop rtp header
      , extensionHeader = None
    )

    (RTPPacketCodec.codec.decode(rtpData) ?= Attempt.successful(DecodeResult(packet, BitVector.empty))) &&
    (RTPPacketCodec.codec.encode(packet) ?= Attempt.successful(rtpData))

  }

  property("encode-decode-padded-3") = protect {
    val packet = RTPPacket(
      version = RTPVersion.V_2
      , marker = false
      , payloadType = 8
      , sequenceNumber = 59930
      , timestamp = 631834285
      , ssrc = 1442973472
      , csrc = Nil
      , payload = ByteVector(Array[Byte](1))
      , extensionHeader = None
    )

    val rtpData = BitVector.fromHex("a008ea1a25a906ad5602072001000003").get

    (RTPPacketCodec.codec.encode(packet) ?= Attempt.successful(rtpData)) &&
    (RTPPacketCodec.codec.decode(rtpData) ?= Attempt.successful(DecodeResult(packet, BitVector.empty)))
  }

  property("encode-decode-padded-2") = protect {
    val packet = RTPPacket(
      version = RTPVersion.V_2
      , marker = false
      , payloadType = 8
      , sequenceNumber = 59930
      , timestamp = 631834285
      , ssrc = 1442973472
      , csrc = Nil
      , payload = ByteVector(Array[Byte](1, 1))
      , extensionHeader = None
    )

    val rtpData = BitVector.fromHex("a008ea1a25a906ad5602072001010002").get

    (RTPPacketCodec.codec.encode(packet) ?= Attempt.successful(rtpData)) &&
      (RTPPacketCodec.codec.decode(rtpData) ?= Attempt.successful(DecodeResult(packet, BitVector.empty)))
  }

  property("encode-decode-padded-1") = protect {
    val packet = RTPPacket(
      version = RTPVersion.V_2
      , marker = false
      , payloadType = 8
      , sequenceNumber = 59930
      , timestamp = 631834285
      , ssrc = 1442973472
      , csrc = Nil
      , payload = ByteVector(Array[Byte](1, 1, 1))
      , extensionHeader = None
    )

    val rtpData = BitVector.fromHex("a008ea1a25a906ad5602072001010101").get

    (RTPPacketCodec.codec.encode(packet) ?= Attempt.successful(rtpData)) &&
      (RTPPacketCodec.codec.decode(rtpData) ?= Attempt.successful(DecodeResult(packet, BitVector.empty)))
  }

  property("encode-decode-padded-0") = protect {
    val packet = RTPPacket(
      version = RTPVersion.V_2
      , marker = false
      , payloadType = 8
      , sequenceNumber = 59930
      , timestamp = 631834285
      , ssrc = 1442973472
      , csrc = Nil
      , payload = ByteVector(Array[Byte](1, 1, 1, 1))
      , extensionHeader = None
    )

    val rtpData = BitVector.fromHex("8008ea1a25a906ad5602072001010101").get

    (RTPPacketCodec.codec.encode(packet) ?= Attempt.successful(rtpData)) &&
      (RTPPacketCodec.codec.decode(rtpData) ?= Attempt.successful(DecodeResult(packet, BitVector.empty)))
  }

  property("encode-decode.csrc-one") = protect {
    val packet = RTPPacket(
      version = RTPVersion.V_2
      , marker = false
      , payloadType = 8
      , sequenceNumber = 59930
      , timestamp = 631834285
      , ssrc = 1442973472
      , csrc = Seq(911)
      , payload = ByteVector(Array[Byte](1, 1, 1, 1))
      , extensionHeader = None
    )

    val rtpData = BitVector.fromHex("8108ea1a25a906ad560207200000038f01010101").get

    (RTPPacketCodec.codec.encode(packet) ?= Attempt.successful(rtpData)) &&
      (RTPPacketCodec.codec.decode(rtpData) ?= Attempt.successful(DecodeResult(packet, BitVector.empty)))

  }

  property("encode-decode-extension-header.content-empty") = protect {
    val packet = RTPPacket(
      version = RTPVersion.V_2
      , marker = false
      , payloadType = 8
      , sequenceNumber = 59930
      , timestamp = 631834285
      , ssrc = 1442973472
      , csrc = Seq(911)
      , payload = ByteVector(Array[Byte](1, 1, 1, 1))
      , extensionHeader = Some(
        RTPHeaderExtension(
          flag = 99
          , content = ByteVector.empty
        )
      )
    )

    encodeDecode(
      packet
      , "9108ea1a25a906ad560207200000038f0063000001010101"
    )
  }

  property("encode-decode-extension-header.content-present") = protect {
    val packet = RTPPacket(
      version = RTPVersion.V_2
      , marker = false
      , payloadType = 8
      , sequenceNumber = 59930
      , timestamp = 631834285
      , ssrc = 1442973472
      , csrc = Seq(911)
      , payload = ByteVector(Array[Byte](1, 1, 1, 1))
      , extensionHeader = Some(
        RTPHeaderExtension(
          flag = 99
          , content = ByteVector(Array[Byte](1,2,3,4))
        )
      )
    )

    encodeDecode(
      packet
      , "9108ea1a25a906ad560207200000038f006300010102030401010101"
    )
  }



}
