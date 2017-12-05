package spinoco.protocol.rtp.codec

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._
import scodec.{Attempt, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.rtp._ 

/**
  * Created by pach on 01/03/17.
  */
object RTCPPacketCodecSpec extends Properties("RTCPPacketCodec"){

  def encodeDecode(packet: RTCPPacket, hexBytes: String) : Prop = {
    val rtpData = BitVector.fromHex(hexBytes).get
    (s"Bytes are aligned : $rtpData" |: (rtpData.bytes.size % 4 ?= 0)) &&
      ( "Encoded" |: (RTCPPacketCodec.codec.encode(packet) ?= Attempt.successful(rtpData))) &&
      ( "Decoded" |: (RTCPPacketCodec.codec.decode(rtpData) ?= Attempt.successful(DecodeResult(packet, BitVector.empty))))
  }

  property("sender-report") = protect {

    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.RTCPSenderReport(
        ssrc = 1442973472
        , ntpTimestampMSW = 616435
        , ntpTimestampLSW = 592705446
        , rtpTimestamp = 633281805
        , packetCount = 9049
        , octetCount = 1447840
        , report = Vector(
          Report(
            ssrc = -1217485782
            , fractionLost = 0
            , packetsLost = 0
            , hiSeqNoReceived = 9042
            , jitter = 32
            , lastSRTimestamp = 0
            , delaySinceLastSR = 0
          )
        )
        , extensions = ByteVector.empty
      )
    )

    encodeDecode(
      packet
      , "81c8000c56020720000967f32353f7a625bf1d0d00002359001617a0b76ea42a0000000000002352000000200000000000000000"
    )

  }


  property("sender-report.with-extension") = protect {

    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.RTCPSenderReport(
        ssrc = 1442973472
        , ntpTimestampMSW = 616435
        , ntpTimestampLSW = 592705446
        , rtpTimestamp = 633281805
        , packetCount = 9049
        , octetCount = 1447840
        , report = Vector(
          Report(
            ssrc = -1217485782
            , fractionLost = 0
            , packetsLost = 0
            , hiSeqNoReceived = 9042
            , jitter = 32
            , lastSRTimestamp = 0
            , delaySinceLastSR = 0
          )
        )
        , extensions = ByteVector(1,2,3,4)
      )
    )

    encodeDecode(
      packet
      , "81c8000d56020720000967f32353f7a625bf1d0d00002359001617a0b76ea42a000000000000235200000020000000000000000001020304"
    )

  }


  property("sender-report.with-extension.pad-1") = protect {

    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.RTCPSenderReport(
        ssrc = 1442973472
        , ntpTimestampMSW = 616435
        , ntpTimestampLSW = 592705446
        , rtpTimestamp = 633281805
        , packetCount = 9049
        , octetCount = 1447840
        , report = Vector(
          Report(
            ssrc = -1217485782
            , fractionLost = 0
            , packetsLost = 0
            , hiSeqNoReceived = 9042
            , jitter = 32
            , lastSRTimestamp = 0
            , delaySinceLastSR = 0
          )
        )
        , extensions = ByteVector(1,2,3)
      )
    )

    encodeDecode(
      packet
      , "a1c8000d56020720000967f32353f7a625bf1d0d00002359001617a0b76ea42a000000000000235200000020000000000000000001020301"
    )

  }

  property("sender-report.with-extension.pad-2") = protect {

    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.RTCPSenderReport(
        ssrc = 1442973472
        , ntpTimestampMSW = 616435
        , ntpTimestampLSW = 592705446
        , rtpTimestamp = 633281805
        , packetCount = 9049
        , octetCount = 1447840
        , report = Vector(
          Report(
            ssrc = -1217485782
            , fractionLost = 0
            , packetsLost = 0
            , hiSeqNoReceived = 9042
            , jitter = 32
            , lastSRTimestamp = 0
            , delaySinceLastSR = 0
          )
        )
        , extensions = ByteVector(1,2)
      )
    )

    encodeDecode(
      packet
      , "a1c8000d56020720000967f32353f7a625bf1d0d00002359001617a0b76ea42a000000000000235200000020000000000000000001020002"
    )

  }


  property("sender-report.with-extension.pad-3") = protect {

    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.RTCPSenderReport(
        ssrc = 1442973472
        , ntpTimestampMSW = 616435
        , ntpTimestampLSW = 592705446
        , rtpTimestamp = 633281805
        , packetCount = 9049
        , octetCount = 1447840
        , report = Vector(
          Report(
            ssrc = -1217485782
            , fractionLost = 0
            , packetsLost = 0
            , hiSeqNoReceived = 9042
            , jitter = 32
            , lastSRTimestamp = 0
            , delaySinceLastSR = 0
          )
        )
        , extensions = ByteVector(1)
      )
    )

    encodeDecode(
      packet
      , "a1c8000d56020720000967f32353f7a625bf1d0d00002359001617a0b76ea42a000000000000235200000020000000000000000001000003"
    )

  }



  property("source-description") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.SourceDescription(
        descriptors = Vector(
          SourceDescriptor(
            ssrc = 1442973472
            , attributes = Vector(
              (SourceDescriptorType.CNAME, "c12")
            )
          )
        )
      )
    )

    encodeDecode(
      packet
      , "81ca0003560207200103633132000000"
    )

  }

  property("source-description.pad-1") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.SourceDescription(
        descriptors = Vector(
          SourceDescriptor(
            ssrc = 1442973472
            , attributes = Vector(
              (SourceDescriptorType.CNAME, "A")
            )
          )
        )
      )
    )

    encodeDecode(
      packet
      , "0x81ca00025602072001014100"
    )

  }

  property("source-description.pad-2") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.SourceDescription(
        descriptors = Vector(
          SourceDescriptor(
            ssrc = 1442973472
            , attributes = Vector(
              (SourceDescriptorType.CNAME, "AA")
            )
          )
        )
      )
    )

    encodeDecode(
      packet
      , "0x81ca0003560207200102414100000000"
    )

  }

  property("source-description.pad-3") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.SourceDescription(
        descriptors = Vector(
          SourceDescriptor(
            ssrc = 1442973472
            , attributes = Vector(
              (SourceDescriptorType.CNAME, "AAA")
            )
          )
        )
      )
    )

    encodeDecode(
      packet
      , "81ca0003560207200103414141000000"
    )

  }

  property("source-description.pad-4") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.SourceDescription(
        descriptors = Vector(
          SourceDescriptor(
            ssrc = 1442973472
            , attributes = Vector(
              (SourceDescriptorType.CNAME, "AAAA")
            )
          )
        )
      )
    )

    encodeDecode(
      packet
      , "81ca0003560207200104414141410000"
    )

  }

  property("source-description.pad-0") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.SourceDescription(
        descriptors = Vector(
          SourceDescriptor(
            ssrc = 1442973472
            , attributes = Vector(
              (SourceDescriptorType.CNAME, "")
            )
          )
        )
      )
    )

    encodeDecode(
      packet
      , "81ca00025602072001000000"
    )

  }


  property("source-description-multiple") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.SourceDescription(
        descriptors = Vector(
          SourceDescriptor(
            ssrc = 1442973472
            , attributes = Vector(
              (SourceDescriptorType.CNAME, "c12")
              , (SourceDescriptorType.EMAIL, "john.doe@email.com")
            )
          )
        )
      )
    )

    encodeDecode(
      packet
      , "81ca000856020720010363313203126a6f686e2e646f6540656d61696c2e636f6d000000"
    )
  }


  property("receiver-report") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.RTCPReceiverReport(
        ssrc = 1442973472
        , report = Vector(
          Report(
            ssrc = -1217485782
            , fractionLost = 0
            , packetsLost = 0
            , hiSeqNoReceived = 9042
            , jitter = 32
            , lastSRTimestamp = 0
            , delaySinceLastSR = 0
          )
        )
        , extensions = ByteVector.empty
      )
    )

    encodeDecode(
      packet
      , "0x81c9000756020720b76ea42a0000000000002352000000200000000000000000"
    )
  }


  property("bye.no-reason") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.Bye(
        ids = Vector(1442973472)
        , reason = None
      )
    )

    encodeDecode(
      packet
      , "0x81cb000156020720"
    )
  }

  property("bye.with-reason") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.Bye(
        ids = Vector(1442973472)
        , reason = Some("reason")
      )
    )

    encodeDecode(
      packet
      , "0xa1cb00035602072030726561736f6e01"
    )
  }

  property("app-data.empty") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.ApplicationData(
        ssrc = 1442973472
        , name = "APP1"
        , data = ByteVector.empty
      )
    )

    encodeDecode(
      packet
      , "0x80cc00025602072041505031"
    )
  }


  property("app-data.non-empty") = protect {
    val packet = RTCPPacket(
      version = RTPVersion.V_2
      , body = RTCPPacketBody.ApplicationData(
        ssrc = 1442973472
        , name = "APP1"
        , data = ByteVector(1, 2)
      )
    )

    encodeDecode(
      packet
      , "0xa0cc0003560207204150503101020002"
    )
  }


}
