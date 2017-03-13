package spinoco.protocol.sdp.codec

import java.net.URI
import java.time.{LocalDateTime, ZoneOffset}

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import spinoco.protocol.sdp.Attribute._
import spinoco.protocol.sdp._

import scala.concurrent.duration._

object SessionDescriptionCodecSpec extends Properties("SessionDescriptionCodec") {

  val now = LocalDateTime.ofEpochSecond(0,0,ZoneOffset.UTC)
  val ntp0 = LocalDateTime.ofEpochSecond(0,0,ZoneOffset.UTC).minusSeconds(2208988800L)



  def decode(s:String, sd: SessionDescription): Prop = {
    s"Decode" |: (SessionDescriptionCodec.codec.decode(BitVector.view(s.getBytes())) ?=
      Attempt.successful(DecodeResult(sd, BitVector.empty)))
  }

  def encode(s:String, sd: SessionDescription): Prop = {
    s"Encode" |: (SessionDescriptionCodec.codec.encode(sd).map(_.decodeUtf8) ?=
      Attempt.successful(Right(s)))
  }

  def decodeAndEncode(s:String)(sd: SessionDescription): Prop = {
    decode(s, sd) && encode(s, sd)
  }

  property("decode-encode.full") = protect {

    val sdp = SessionDescription(
      version = 0
      , origin = SessionOrigin("john.doe@spinoco.com", "1234", "56789",NetType.IN, AddressType.IP4, "1.2.3.4")
      , name = "session-test"
      , information = Some("Session Information")
      , uri = Some(URI.create("http://www.spinoco.com"))
      , email = List("john.doe@spinoco.com")
      , phone = List("+420222500500")
      , connectionData = List(ConnectionData(NetType.IN, AddressType.IP4, "1.2.3.4", Some(1), Some(2)))
      , bandwidth =  List(Bandwidth("BW1", 1024))
      , timing =  List(Timing(now, now.plusHours(1)))
      , repeat= List(Repeat(1.minute, 10.second, Vector(5.minutes)))
      , zones = List(TimeZone(now, 1.hour))
      , attributes = List(
        Category("cat1")
        , Keywords("keyword1")
        , Tool("toolA")
        , PacketTime(20.millis)
        , MaxPacketTime(40.millis)
        , RtpMap(0, "G711", 8000, None)
        , MediaDirection(MediaDirectionType.SendAndReceive)
        , Orientation(OrientationType.Landscape)
        , ConferenceType("conference")
        , CharacterSet("utf-8")
        , Language("en-us")
        , SDPLanguage("en-us")
        , FrameRate(60, None)
        , Quality(10)
        , FormatParams("G711", "params1")
      )
      , media = List(
        MediaDescription(
          tpe = MediaType.Audio
          , port = 22000
          , portCount = None
          , protocol = MediaProtocol.`RTP/AVP`
          , format = List(0, 10)
          , information = Some("Media info")
          , connectionData = List(ConnectionData(NetType.IN, AddressType.IP4, "1.2.1.2", None, None))
          , bandwidth = List(Bandwidth("BWX", 512))
          , attributes = List(
            PacketTime(20.millis)
            , MaxPacketTime(40.millis)
          )
        )
        , MediaDescription(
          tpe = MediaType.Video
          , port = 22003
          , portCount = None
          , protocol = MediaProtocol.`RTP/AVP`
          , format = List(100, 200)
          , information = Some("Media info video")
          , connectionData = List(ConnectionData(NetType.IN, AddressType.IP4, "1.2.3.3", None, None))
          , bandwidth = List(Bandwidth("BWW", 2048))
          , attributes = List(
            PacketTime(20.millis)
            , MaxPacketTime(40.millis)
          )
        )
      )
    )


    decodeAndEncode(
      """v=0
        |o=john.doe@spinoco.com 1234 56789 IN IP4 1.2.3.4
        |s=session-test
        |i=Session Information
        |u=http://www.spinoco.com
        |e=john.doe@spinoco.com
        |p=+420222500500
        |c=IN IP4 1.2.3.4/1/2
        |b=BW1:1024
        |t=2208988800 2208992400
        |r=60 10 300
        |z=2208988800 3600
        |a=cat:cat1
        |a=keywds:keyword1
        |a=tool:toolA
        |a=ptime:20
        |a=maxptime:40
        |a=rtpmap:0 G711/8000
        |a=sendrecv
        |a=orient:landscape
        |a=type:conference
        |a=charset:utf-8
        |a=lang:en-us
        |a=sdplang:en-us
        |a=framerate:60
        |a=quality:10
        |a=fmtp:G711 params1
        |m=audio 22000 RTP/AVP 0 10
        |i=Media info
        |c=IN IP4 1.2.1.2
        |b=BWX:512
        |a=ptime:20
        |a=maxptime:40
        |m=video 22003 RTP/AVP 100 200
        |i=Media info video
        |c=IN IP4 1.2.3.3
        |b=BWW:2048
        |a=ptime:20
        |a=maxptime:40
        |""".stripMargin.replace("\n", "\r\n")
    )(sdp)


  }


  property("decode-encode.alice.1") = protect {
    decodeAndEncode(
      """v=0
        |o=alice 2890844526 2890844526 IN IP4 host.atlanta.example.com
        |s=X
        |c=IN IP4 host.atlanta.example.com
        |t=0 0
        |m=audio 49170 RTP/AVP 0 8 97
        |a=rtpmap:0 PCMU/8000
        |a=rtpmap:8 PCMA/8000
        |a=rtpmap:97 iLBC/8000
        |m=video 51372 RTP/AVP 31 32
        |a=rtpmap:31 H261/90000
        |a=rtpmap:32 MPV/90000
        |""".stripMargin.replace("\n", "\r\n")
    )(
      SessionDescription(
        version = 0
        , origin = SessionOrigin("alice", "2890844526", "2890844526", NetType.IN, AddressType.IP4, "host.atlanta.example.com")
        , name = "X"
      ).copy(
        connectionData = List(ConnectionData(NetType.IN, AddressType.IP4, "host.atlanta.example.com", None, None))
        , timing = List(Timing(ntp0, ntp0))
        , media = List(
          MediaDescription(
            tpe = MediaType.Audio
            , port = 49170
            , portCount = None
            , protocol = MediaProtocol.`RTP/AVP`
            , format = List(0, 8, 97)
            , information = None
            , connectionData = Nil
            , bandwidth = Nil
            , attributes = List(
              RtpMap(0, "PCMU", 8000, None)
              , RtpMap(8, "PCMA", 8000, None)
              , RtpMap(97, "iLBC", 8000, None)
            )
          )
          , MediaDescription(
            tpe = MediaType.Video
            , port = 51372
            , portCount = None
            , protocol = MediaProtocol.`RTP/AVP`
            , format = List(31, 32)
            , information = None
            , connectionData = Nil
            , bandwidth = Nil
            , attributes = List(
              RtpMap(31, "H261", 90000, None)
              , RtpMap(32, "MPV", 90000, None)
            )
          )
        )
      )
    )

  }

  property("decode-encode-simple-mgcp") = protect {
    decode(
      """v=0
        |o=- 25678 753849 IN IP4 128.96.41.1
        |s=-
        |c=IN IP4 128.96.41.1
        |t=0 0
        |m=audio 3456 RTP/AVP 0
        |""".stripMargin
      , SessionDescription(
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
    )
  }

}
