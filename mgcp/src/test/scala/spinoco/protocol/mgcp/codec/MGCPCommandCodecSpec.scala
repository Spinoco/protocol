package spinoco.protocol.mgcp.codec

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._
import scodec.{Attempt, DecodeResult}
import shapeless.tag
import spinoco.protocol.mgcp._
import LocalEndpointPart._
import scodec.bits.BitVector
import spinoco.protocol.mgcp.LocalConnectionOption.{CompressionAlgorithm, PacketizationPeriod}
import spinoco.protocol.mgcp.MGCPParameter._
import spinoco.protocol.mgcp.mgcppackage.{DTMFPackageEvent,  DTMFPackagePatternEvent, GenericPackageEvent, LinePackageEvent}
import spinoco.protocol.sdp._


object MGCPCommandCodecSpec extends Properties("MGCPCommandCodec"){

  def encode(command: MGCPCommand)(encoded: String): Prop = {
    "Encode" |: (MGCPCommandCodec.codec.encode(command).map(_.decodeUtf8) ?= Attempt.successful(Right(encoded.lines.mkString("", "\r\n", "\r\n"))))
  }

  def decode(command: MGCPCommand)(encoded: String): Prop = {
    "Decode" |: (MGCPCommandCodec.codec.decode(BitVector.view(encoded.getBytes)) ?= Attempt.successful(DecodeResult(command, BitVector.empty)))
  }

  def decodeAndEncode(command: MGCPCommand)(encoded: String): Prop = {
    decode(command)(encoded) &&
    encode(command)(encoded)
  }

  val commonSdp  =
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


  property("RFC.F.1.NotificationRequest.a") = protect {
    // NotificationRequest that will ring a phone and look for an off-hook event:
    decodeAndEncode(MGCPCommand(
      tpe =  MGCPCommandType.RQNT
      , txId = tag[MGCPTxId](1201)
      , endpoint = LocalEndpointName(NameString("aaln"), List(NameString("1")))
      , domain = "rgw-2567.whatever.net"
      , version = MGCPVersion(1, 0, None)
      , params = List(
        NotifiedEntity(EntityName(Some(LocalEndpointName(NameString("ca"), Nil)), "ca1.whatever.net", Some(5678)))
        , RequestIdentifier(tag[RequestIdentifier]("0123456789AC"))
        , RequestedEvents(List(RequestedEvent(EventSpecification(LinePackageEvent.OffHookTransition, None), List(RequestedEventAction.NotifyImmediately))))
        , SignalRequests(List(ParametrizedEvent(EventSpecification(LinePackageEvent.Ringing, None), None)))
      )
      , sdp = None
    ))(
      """RQNT 1201 aaln/1@rgw-2567.whatever.net MGCP 1.0
        |N: ca@ca1.whatever.net:5678
        |X: 0123456789AC
        |R: l/hd(N)
        |S: l/rg
        |""".stripMargin)
  }

  property("RFC.F.1.NotificationRequest.b") = protect {
    // NotificationRequest that will look
    // for and accumulate an off-hook event, and then provide dial-tone and
    // accumulate digits according to the digit map provided
    val command = MGCPCommand(
      tpe =  MGCPCommandType.RQNT
      , txId = tag[MGCPTxId](1202)
      , endpoint = LocalEndpointName(NameString("aaln"), List(NameString("1")))
      , domain = "rgw-2567.whatever.net"
      , version = MGCPVersion(1, 0, None)
      , params = List(
        NotifiedEntity(EntityName(Some(LocalEndpointName(NameString("ca"), Nil)), "ca1.whatever.net", Some(5678)))
        , RequestIdentifier(tag[RequestIdentifier]("0123456789AC"))
        , RequestedEvents(List(
          RequestedEvent(
            EventSpecification(LinePackageEvent.OffHookTransition, None)
            , List(
              RequestedEventAction.Accumulate
              , RequestedEventAction.EmbeddedRequest(
                requests = List(
                  RequestedEvent(EventSpecification(LinePackageEvent.OperationComplete, None), Nil)
                  , RequestedEvent(EventSpecification(LinePackageEvent.OnHookTransition, None), Nil)
                  , RequestedEvent(EventSpecification(DTMFPackagePatternEvent("0-9#*T"), None), List(RequestedEventAction.TreatByDigitMap))
                )
                , signalled = List(ParametrizedEvent(EventSpecification(LinePackageEvent.DialTone, None), None))
                , digitMap = None
              )
            )
          )
        ))
        , DigitMap("(0T|00T|#xxxxxxx|*xx|91xxxxxxxxxx|9011x.T)")
        , SignalRequests(List())
        , QuarantineHandling(List(QuarantineHandlingStyle.ProcessControlProcess))
        , DetectEvents(List(ParametrizedEvent(EventSpecification(GenericPackageEvent.FaxTone, None), None)))
      )
      , sdp = None
    )

    val encoded =
      """RQNT 1202 aaln/1@rgw-2567.whatever.net MGCP 1.0
        |N: ca@ca1.whatever.net:5678
        |X: 0123456789AC
        |R: l/hd(A, E(R(l/oc, l/hu, d/[0-9#*T](D)), S(l/dl)))
        |D: (0T|00T|#xxxxxxx|*xx|91xxxxxxxxxx|9011x.T)
        |S:""".stripMargin + " " + """
        |Q: process
        |T: g/ft
        |""".stripMargin

    val decoded =
      """RQNT 1202 aaln/1@rgw-2567.whatever.net MGCP 1.0
        |N: ca@ca1.whatever.net:5678
        |X: 0123456789AC
        |R: L/hd(A, E(S(L/dl),R(L/oc, L/hu, D/[0-9#*T](D))))
        |D: (0T|00T|#xxxxxxx|*xx|91xxxxxxxxxx|9011x.T)
        |S:
        |Q: process
        |T: G/ft
        |""".stripMargin

    decode(command)(decoded) &&
      encode(command)(encoded)
  }


  property("RFC.F.2.Notify") = protect {
    val command = MGCPCommand(
      tpe = MGCPCommandType.NTFY
      , txId = tag[MGCPTxId](2002)
      , endpoint = LocalEndpointName(NameString("aaln"), List(NameString("1")))
      , domain = "rgw-2567.whatever.net"
      , version = MGCPVersion(1, 0, None)
      , params = List(
        NotifiedEntity(EntityName(Some(LocalEndpointName(NameString("ca"), Nil)), "ca1.whatever.net", Some(5678)))
        , RequestIdentifier(tag[RequestIdentifier]("0123456789AC"))
        , ObservedEvents(List(
          EventSpecification(LinePackageEvent.OffHookTransition, None)
          , EventSpecification(DTMFPackageEvent.DTMF9, None)
          , EventSpecification(DTMFPackageEvent.DTMF1, None)
          , EventSpecification(DTMFPackageEvent.DTMF2, None)
          , EventSpecification(DTMFPackageEvent.DTMF0, None)
          , EventSpecification(DTMFPackageEvent.DTMF1, None)
          , EventSpecification(DTMFPackageEvent.DTMF8, None)
          , EventSpecification(DTMFPackageEvent.DTMF2, None)
          , EventSpecification(DTMFPackageEvent.DTMF9, None)
          , EventSpecification(DTMFPackageEvent.DTMF4, None)
          , EventSpecification(DTMFPackageEvent.DTMF2, None)
          , EventSpecification(DTMFPackageEvent.DTMF6, None)
          , EventSpecification(DTMFPackageEvent.DTMF6, None)
        ).map(ParametrizedEvent(_, None)))
      )
      , sdp = None
    )
    val encoded =
      """NTFY 2002 aaln/1@rgw-2567.whatever.net MGCP 1.0
        |N: ca@ca1.whatever.net:5678
        |X: 0123456789AC
        |O: l/hd,d/9,d/1,d/2,d/0,d/1,d/8,d/2,d/9,d/4,d/2,d/6,d/6
        |""".stripMargin

    val decoded =
      """NTFY 2002 aaln/1@rgw-2567.whatever.net MGCP 1.0
        |N: ca@ca1.whatever.net:5678
        |X: 0123456789AC
        |O: L/hd,D/9,D/1,D/2,D/0,D/1,D/8,D/2,D/9,D/4,D/2,D/6,D/6
        |""".stripMargin
    decode(command)(decoded) &&
    encode(command)(encoded)
  }


  property("RFC.F.3.CreateConnection.a") = protect {
    decode(MGCPCommand(
      tpe = MGCPCommandType.CRCX
      , txId = tag[MGCPTxId](1204)
      , endpoint = LocalEndpointName(NameString("aaln"), List(NameString("1")))
      , domain = "rgw-2567.whatever.net"
      , version = MGCPVersion(1, 0, None)
      , params = List(
        CallId(tag[CallId]("A3C47F21456789F0"))
        , LocalConnectionOptions(List(PacketizationPeriod(10), CompressionAlgorithm("PCMU")))
        , ConnectionMode(ConnectionModeType.ReceiveOnly)
      )
      , sdp = None
    ))(
      """CRCX 1204 aaln/1@rgw-2567.whatever.net MGCP 1.0
        |C: A3C47F21456789F0
        |L: p:10, a:PCMU
        |M: recvonly
        |""".stripMargin)
  }

  property("RFC.F.3.CreateConnection.b") = protect {

    val command = MGCPCommand(
      tpe = MGCPCommandType.CRCX
      , txId = tag[MGCPTxId](1205)
      , endpoint = LocalEndpointName(NameString("aaln"), List(NameString("1")))
      , domain = "rgw-2567.whatever.net"
      , version = MGCPVersion(1, 0, None)
      , params = List(
        CallId(tag[CallId]("A3C47F21456789F0"))
        , LocalConnectionOptions(List(PacketizationPeriod(10), CompressionAlgorithm("PCMU")))
        , ConnectionMode(ConnectionModeType.SendAndReceive)
        , RequestIdentifier(tag[RequestIdentifier]("0123456789AD"))
        , RequestedEvents(List(RequestedEvent(EventSpecification(LinePackageEvent.OffHookTransition, None), Nil)))
        , SignalRequests(List(ParametrizedEvent(EventSpecification(LinePackageEvent.Ringing, None), None)))
      )
      , sdp = Some(commonSdp)
    )

    val toDecode =
      """CRCX 1205 aaln/1@rgw-2567.whatever.net MGCP 1.0
        |C: A3C47F21456789F0
        |L: p:10, a:PCMU
        |M: sendrecv
        |X: 0123456789AD
        |R: L/hd
        |S: L/rg
        |
        |v=0
        |o=- 25678 753849 IN IP4 128.96.41.1
        |s=-
        |c=IN IP4 128.96.41.1
        |t=0 0
        |m=audio 3456 RTP/AVP 0
        |""".stripMargin

    val encoded =
      """CRCX 1205 aaln/1@rgw-2567.whatever.net MGCP 1.0
        |C: A3C47F21456789F0
        |L: p:10,a:PCMU
        |M: sendrecv
        |X: 0123456789AD
        |R: l/hd
        |S: l/rg
        |
        |v=0
        |o=- 25678 753849 IN IP4 128.96.41.1
        |s=-
        |c=IN IP4 128.96.41.1
        |t=0 0
        |m=audio 3456 RTP/AVP 0
        |""".stripMargin

    decode(command)(toDecode) &&
    encode(command)(encoded)
  }

  property("RFC.F.3.CreateConnection.c") = protect {

    val command = MGCPCommand(
      tpe = MGCPCommandType.CRCX
      , txId = tag[MGCPTxId](1206)
      , endpoint = LocalEndpointName(NameString("aaln"), List(NameString("1")))
      , domain = "rgw-2569.whatever.net"
      , version = MGCPVersion(1, 0, None)
      , params = List(
        ResponseAck(List(AckTx(tag[MGCPTxId](1205), None)))
        , CallId(tag[CallId]("A3C47F21456789F0"))
        , LocalConnectionOptions(List(PacketizationPeriod(10), CompressionAlgorithm("PCMU")))
        , ConnectionMode(ConnectionModeType.Inactive)
      )
      , sdp = Some(commonSdp)
    )


    val toDecode =
      """CRCX 1206 aaln/1@rgw-2569.whatever.net MGCP 1.0
        |K: 1205
        |C: A3C47F21456789F0
        |L: p:10, a:PCMU
        |M: inactive
        |
        |v=0
        |o=- 25678 753849 IN IP4 128.96.41.1
        |s=-
        |c=IN IP4 128.96.41.1
        |t=0 0
        |m=audio 3456 RTP/AVP 0
        |""".stripMargin

    val encoded =
      """CRCX 1206 aaln/1@rgw-2569.whatever.net MGCP 1.0
         |K: 1205
         |C: A3C47F21456789F0
         |L: p:10,a:PCMU
         |M: inactive
         |
         |v=0
         |o=- 25678 753849 IN IP4 128.96.41.1
         |s=-
         |c=IN IP4 128.96.41.1
         |t=0 0
         |m=audio 3456 RTP/AVP 0
         |""".stripMargin

    decode(command)(toDecode) &&
    encode(command)(encoded)
  }
}
