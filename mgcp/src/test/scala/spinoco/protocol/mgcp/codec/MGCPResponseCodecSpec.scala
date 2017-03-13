package spinoco.protocol.mgcp.codec

import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import shapeless.tag
import spinoco.protocol.mgcp.MGCPParameter.{ConnectionId, ResponseAck}
import spinoco.protocol.mgcp.{MGCPResponse, MGCPResponseCode, MGCPTxId}
import spinoco.protocol.sdp._


object MGCPResponseCodecSpec extends Properties("MGCPResponseCodec") {

  def encode(command: MGCPResponse)(encoded: String): Prop = {
    MGCPResponseCodec.codec.encode(command).map(_.decodeUtf8) ?= Attempt.successful(Right(encoded.replace("\n", "\r\n")))
  }

  def decode(command: MGCPResponse)(encoded: String): Prop = {
    MGCPResponseCodec.codec.decode(BitVector.view(encoded.getBytes)) ?= Attempt.successful(DecodeResult(command, BitVector.empty))
  }

  def decodeAndEncode(command: MGCPResponse)(encoded: String): Prop = {
    ("Decode" |: decode(command)(encoded)) &&
      ("Encode" |: encode(command)(encoded))
  }

  val commonSdp =
    SessionDescription(
      version = 0
      , origin = SessionOrigin("-", "25678", "753849", NetType.IN, AddressType.IP4, "128.96.41.1")
      , name = "-"
    ).copy(
      connectionData = List(
        ConnectionData(NetType.IN, AddressType.IP4, "128.96.41.1", None, None)

      )
      , timing = List(Timing(Timing.startOfEpoch, Timing.startOfEpoch))
      , media = List(
        MediaDescription(
          tpe = MediaType.Audio
          , port = 3456
          , portCount = None
          , protocol = MediaProtocol.`RTP/AVP`
          , format = List(0)
          , information = None
          , connectionData = Nil
          , bandwidth = Nil
          , attributes = Nil
        )
      )
    )

  property("RFC.F.1.NotificationRequest") = protect {
    decode(
      MGCPResponse(
        code = MGCPResponseCode.Ok
        , txId = tag[MGCPTxId](1201)
        , packageName = None
        , responseString = Some("OK")
        , params = Nil
        , sdp = None
      )
    )(
      """200 1201 OK
        |""".stripMargin
    )
  }


  property("RFC.F.3.CreateConnection.a") = protect {
    decodeAndEncode(
      MGCPResponse(
        code = MGCPResponseCode.Ok
        , txId = tag[MGCPTxId](1204)
        , packageName = None
        , responseString = Some("OK")
        , params = List(
          ConnectionId(tag[ConnectionId]("FDE234C8"))
        )
        , sdp = Some(commonSdp)
      )
    )(
      """200 1204 OK
        |I: FDE234C8
        |
        |v=0
        |o=- 25678 753849 IN IP4 128.96.41.1
        |s=-
        |c=IN IP4 128.96.41.1
        |t=0 0
        |m=audio 3456 RTP/AVP 0
        |""".stripMargin)
  }


  property("RFC.F.3.CreateConnection.b") = protect {
    decode(
      MGCPResponse(
        code = MGCPResponseCode.AlreadyOffHook
        , txId = tag[MGCPTxId](1205)
        , packageName = None
        , responseString = Some("Phone off-hook")
        , params = Nil
        , sdp = None
      )
    )(
      """401 1205 Phone off-hook
        |""".stripMargin
    )
  }

  property("RFC.F.3.CreateConnection.c.provisional") = protect {
    decode(
      MGCPResponse(
        code = MGCPResponseCode.InProgress
        , txId = tag[MGCPTxId](1206)
        , packageName = None
        , responseString = Some("Pending")
        , params = List(
          ConnectionId(tag[ConnectionId]("DFE233D1"))
        )
        , sdp = Some(commonSdp)
      )
    )(
      """100 1206 Pending
        |I: DFE233D1
        |
        |v=0
        |o=- 25678 753849 IN IP4 128.96.41.1
        |s=-
        |c=IN IP4 128.96.41.1
        |t=0 0
        |m=audio 3456 RTP/AVP 0
        |""".stripMargin
    )
  }


  property("RFC.F.3.CreateConnection.c.final") = protect {
    decode(
      MGCPResponse(
        code = MGCPResponseCode.Ok
        , txId = tag[MGCPTxId](1206)
        , packageName = None
        , responseString = Some("OK")
        , params = List(
          ResponseAck(Nil)
          , ConnectionId(tag[ConnectionId]("DFE233D1"))
        )
        , sdp = Some(commonSdp)
      )
    )(
      """200 1206 OK
        |K:
        |I: DFE233D1
        |
        |v=0
        |o=- 25678 753849 IN IP4 128.96.41.1
        |s=-
        |c=IN IP4 128.96.41.1
        |t=0 0
        |m=audio 3456 RTP/AVP 0
        |""".stripMargin
    )
  }

}
