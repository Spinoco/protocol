package spinoco.protocol.sdp.codec

import java.net.URI
import java.time.{LocalDateTime, ZoneOffset}

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._
import scodec.bits.BitVector
import shapeless.{::, HNil}
import spinoco.protocol.sdp._
import spinoco.protocol.common.codec._
import spinoco.protocol.common.util._

import scala.annotation.tailrec
import scala.concurrent.duration._

object SessionDescriptionCodec {
  val lf: Byte = '\n'
  val cr: Byte = '\r'
  val `\r\n` = BitVector.view(Array(cr,lf))
  val `\n` = BitVector.view(Array(lf))
  val EOL = choice(
    constant(`\r\n`)
    , constant(`\n`)
  )


  lazy val codec: Codec[SessionDescription] = {
    import impl._




    new Codec[SessionDescription] {
      def decode(bits: BitVector): Attempt[DecodeResult[SessionDescription]] = {
        @tailrec
        def go(remains: BitVector, curr: SessionDescription, ln: Int): Attempt[DecodeResult[SessionDescription]] = {
          if (remains.isEmpty) Attempt.successful(DecodeResult(curr, BitVector.empty))
          else {
            if (remains.size <= 16) Attempt.failure(Err.General(s"Expected ?=, but got ${remains.decodeAscii}", List(ln.toString)))
            else {
              val decoded =
                remains.getByte(0) match {
                  case 'i' => sessionInformation.decode(remains) map { _ map { i => curr.copy(information = Some(i)) } }
                  case 'u' => sessionUri.decode(remains) map { _ map { uri => curr.copy(uri = Some(uri)) } }
                  case 'e' => email.decode(remains) map { _ map { email => curr.copy(email = curr.email :+ email) } }
                  case 'p' => phone.decode(remains) map { _ map { phone => curr.copy(phone = curr.phone :+ phone) } }
                  case 'c' => connectionData.decode(remains) map { _ map { cd => curr.copy(connectionData = curr.connectionData :+ cd) } }
                  case 'b' => bandwidth.decode(remains) map { _ map { bw => curr.copy(bandwidth = curr.bandwidth :+ bw) } }
                  case 't' => timing.decode(remains) map { _ map { t => curr.copy(timing = curr.timing :+ t) } }
                  case 'r' => repeat.decode(remains) map { _ map { r => curr.copy(repeat = curr.repeat :+ r) } }
                  case 'z' => zoneOffset.decode(remains) map { _ map { tz => curr.copy(zones = curr.zones :+ tz) } }
                  case 'a' => attribute.decode(remains) map { _ map { attr => curr.copy(attributes = curr.attributes :+ attr) } }
                  case 'm' => media.decode(remains) map { _ map { md => curr.copy(media = curr.media :+ md) } }
                  case other => Attempt.Failure(Err(s"Unexpected Session parameter: '${other.toChar}'"))
                }
              decoded match {
                case Attempt.Successful(DecodeResult(next, rem)) =>
                  // assure the next part start with either lf or cr/lf
                  if (rem.startsWith(`\r\n`)) go(rem.drop(16), next, ln + 1)
                  else if (rem.startsWith(`\n`)) go(rem.drop(8), next, ln + 1)
                  else if (rem.isEmpty) go(rem, next, ln + 1)
                  else Attempt.Failure(Err(s"Each line of sdp must be separated by \\r\\n or \\n ($ln), ${rem.decodeUtf8}"))

                case Attempt.Failure(err) => Attempt.failure(err.pushContext(ln.toString))
              }
            }
          }
        }


        (version <~ EOL).decode(bits).flatMap { vdr =>
        (origin <~ EOL).decode(vdr.remainder).flatMap { odr =>
        (sessionName <~ EOL).decode(odr.remainder).flatMap { sndr =>
          go(sndr.remainder, SessionDescription(vdr.value, odr.value, sndr.value), 3)
        }}}

      }



      def encode(sd: SessionDescription): Attempt[BitVector] = {
        version.encode(sd.version)
        .nextLine(origin.encode(sd.origin))
        .nextLine(sessionName.encode(sd.name))
        .nextLines(sessionInformation)(sd.information.toList)
        .nextLines(sessionUri)(sd.uri.toList)
        .nextLines(email)(sd.email)
        .nextLines(phone)(sd.phone)
        .nextLines(connectionData)(sd.connectionData)
        .nextLines(bandwidth)(sd.bandwidth)
        .nextLines(timing)(sd.timing)
        .nextLines(repeat)(sd.repeat)
        .nextLines(zoneOffset)(sd.zones)
        .nextLines(attribute)(sd.attributes)
        .nextLines(media)(sd.media)
          .map(_ ++ `\r\n`)
      }

      def sizeBound: SizeBound = SizeBound.unknown
    }
  }



  object impl {
    val `=`: Codec[Unit] =  constant(BitVector(Array[Byte]('=')))

    val CrLf: Codec[Unit] =  constant(BitVector(Array[Byte]('\r','\n')))
    val Lf: Codec[Unit] =  constant(BitVector(Array[Byte]('\n')))

    val WS: Codec[Unit] = dropWhile(BitVector(Array[Byte](' ')))(_.toChar.isWhitespace)


    implicit class BitVectorAttemptSyntax(val self: Attempt[BitVector]) extends AnyVal {
      def nextLine(other: Attempt[BitVector]): Attempt[BitVector] = {
        self.flatMap { selfBs =>
        other.map { otherBs =>
          if (selfBs.isEmpty) otherBs
          else if (otherBs.isEmpty) selfBs
          else selfBs ++ `\r\n` ++ otherBs
        }}
      }

      def nextLines[A](codec:Codec[A])(la: List[A]): Attempt[BitVector] =
        nextLine(mkLines(codec)(la))

    }


    // v= parameter
    val version: Codec[Int] =
      parameterLine("Version", 'v')(constant(BitVector(Array[Byte]('0'))).xmap(_ => 0, _ => ()))

    val netTypeEnum = mappedEnum(ascii, NetType.values.map(v => v -> v.toString).toMap)
    val addrTypeEnum = mappedEnum(ascii, AddressType.values.map(v => v -> v.toString).toMap)
    val mediaTypeEnum = mappedEnum(ascii, MediaType.values.map(v => v -> v.toString).toMap)
    val mediaProtocolEnum = mappedEnum(ascii, MediaProtocol.values.map(v => v -> v.toString).toMap)

    // codec that encodes ntp time expressed as long encoded as string
    val ntpTime : Codec[LocalDateTime] = {
      val ntpOffset = 2208988800L
      longAsString.xmap(
        l => LocalDateTime.ofEpochSecond(l - ntpOffset, 0, ZoneOffset.UTC)
        , { ldt => ldt.toEpochSecond(ZoneOffset.UTC) + ntpOffset }
      )
    }

    // codec encoding time duration of various time units, default is seconds
    val timeDuration: Codec[FiniteDuration] = {
      ascii.exmap(
        { s =>
          if (s.endsWith("d")) attempt { s.init.toInt.days }
          else if (s.endsWith("h")) attempt { s.init.toInt.hours }
          else if (s.endsWith("m")) attempt { s.init.toLong.minutes }
          else if (s.endsWith("s")) attempt { s.init.toLong.seconds }
          else attempt { s.toLong.seconds }
        }
        , { d => Attempt.successful { d.toSeconds.toString } }
      )
    }



    def parameterLine[A](prefix: String, discriminator: Char)(codec:Codec[A]):Codec[A] = {
      val disc: Byte = discriminator.toByte
      s"$prefix ($discriminator)" | (
        constant(BitVector(Array[Byte](disc))) ~>
        constant(BitVector(Array[Byte]('='))) ~>
        takeWhile(codec)(b => b != lf && b != cr)
      )
    }




    def mkLines[A](codec: Codec[A])(la: List[A]): Attempt[BitVector] = {
      @tailrec
      def go(rem: List[A], encoded: BitVector): Attempt[BitVector] = {
        rem.headOption match {
          case None => Attempt.successful(encoded)
          case Some(a) => codec.encode(a) match {
            case Attempt.Failure(err) => Attempt.failure(err)
            case Attempt.Successful(bs) =>
              if (encoded.isEmpty) go(rem.tail, bs)
              else if (bs.isEmpty) go(rem.tail, encoded)
              else go(rem.tail, encoded ++ `\r\n` ++ bs)
          }
        }
      }

      go(la, BitVector.empty)
    }

    def encodePList[A](codec:Codec[A])(list: List[A]):Attempt[Vector[BitVector]] = {
      @tailrec
      def go(rem: List[A], acc: Vector[BitVector]):Attempt[Vector[BitVector]] = {
        rem.headOption match {
          case None => Attempt.successful(acc)
          case Some(a) => codec.encode(a) match {
            case Attempt.Failure(err) => Attempt.failure(err)
            case Attempt.Successful(bs) => go(rem.tail, acc :+ bs)
          }
        }
      }

      go(list, Vector.empty)
    }

    def untilWs[A](codec:Codec[A]):Codec[A] =
      takeWhile(codec)(! _.toChar.isWhitespace)

    // o= parameter
    val origin: Codec[SessionOrigin] = parameterLine("Origin", 'o') {
      def isDigit(s: String):Option[Err] = {
        if (s.forall(_.isDigit)) None
        else Some(Err(s"Expected string of digits, but got $s"))
      }


      val userName: Codec[String] = untilWs(ascii)
      val sessionId: Codec[String] = untilWs(guard(ascii)(isDigit))
      val sessionVersion: Codec[String] = untilWs(guard(ascii)(isDigit))
      val netType: Codec[NetType.Value] = untilWs(netTypeEnum)
      val addrType: Codec[AddressType.Value] = untilWs(addrTypeEnum)
      val address: Codec[String] = ascii

      (
        ("UserName" | userName) :: WS ::
          ("Session ID" | sessionId) :: WS ::
          ("Session Version" | sessionVersion) :: WS ::
          ("Network Type" | netType) :: WS ::
          ("Address Type" | addrType) :: WS ::
          ("Address" | address)
      ).dropUnits.as[SessionOrigin]
    }

    // s= parameter
    val sessionName: Codec[String] = parameterLine("Session Name", 's')(guard(ascii) { s =>
      if (s.isEmpty) Some(Err("Session name must not be empty"))
      else None
    })

    // i= parameter
    val sessionInformation : Codec[String] = parameterLine("Information", 'i')(utf8)

    // u= parameter
    val sessionUri: Codec[URI] = parameterLine("Session URI", 'u'){
      utf8.exmap(
        s => attempt { URI.create(s) }
        , uri => Attempt.successful(uri.toString)
      )
    }

    // e= email
    val email: Codec[String] = parameterLine("Email", 'e')(utf8)

    // p= phone
    val phone: Codec[String] =  parameterLine("Phone", 'p')(utf8)

    // c=
    val connectionData: Codec[ConnectionData] = {
      val addressDataCodec =
      listDelimited(BitVector(Array[Byte]('/')), ascii)
      .exmap[String :: Option[Int] :: Option[Int] :: HNil](
        { segments =>
          if (segments.size == 1) Attempt.successful(segments(0) :: None :: None :: HNil)
          else if (segments.size == 2) attempt(segments(1).toInt).map { ttl =>
            segments(0) :: Some(ttl) :: None :: HNil
          } else if (segments.size == 3) {
            attempt(segments(1).toInt).flatMap { ttl =>
            attempt(segments(2).toInt).map { count =>
              segments(0) :: Some(ttl) :: Some(count) :: HNil
            }}
          } else Attempt.failure(Err(s"Expected address/ttl/count, but got $segments"))
        }
         , { case address :: maybeTTl :: maybeCount :: HNil =>
            Attempt.successful {
              List(address) ++ maybeTTl.map(_.toString) ++ maybeCount.map(_.toString)
            }
        }
      )

      parameterLine("Connection Data", 'c')(
        (("Network Type" | untilWs(netTypeEnum)) :: WS ::
          ("Address Type" | untilWs(addrTypeEnum)) :: WS ).dropUnits :::
          ("Address" | addressDataCodec)
      ).as[ConnectionData]
    }

    // b=
    val bandwidth: Codec[Bandwidth] = parameterLine("Bandwidth", 'b'){
      (takeWhile(ascii)(_.toChar != ':') ::
        constant(BitVector(Array[Byte](':'))) ::
        intAsString
      ).dropUnits.as[Bandwidth]
    }

    // t=
    val timing: Codec[Timing] = parameterLine("Timing", 't') {
      (("start" | untilWs(ntpTime)) :: WS ::
        ("stop" | untilWs(ntpTime))
      ).dropUnits.as[Timing]
    }

    // r=
    val repeat: Codec[Repeat] = {
      parameterLine("Repeat", 'r')(
        (
          ("repeat" | untilWs(timeDuration)) :: WS ::
          ("active" | untilWs(timeDuration)) :: WS ::
          ("offsets" | vectorDelimited(BitVector(Array[Byte](' ')), timeDuration))
        ).dropUnits.as[Repeat]
      )
    }

    // z=
    val zoneOffset: Codec[TimeZone] = {
      parameterLine("Zone", 'z')(
        ("Adjustment time" | untilWs(ntpTime)) :: WS ::
          ("offset" | timeDuration)
      ).dropUnits.as[TimeZone]
    }

    // a=
    val attribute: Codec[Attribute] = parameterLine("Attribute", 'a') {
      import Attribute._
      val colon : Byte = ':'
      val slash : Byte = '/'
      val `:` = constant(BitVector.view(Array[Byte](colon)))
      val attributePrefix: Codec[String] = takeWhile(ascii)(_ != colon)

      val msCodec: Codec[FiniteDuration] = intAsString.xmap(_.millis, _.toMillis.toInt)

      val categoryCodec: Codec[Attribute] = (`:` ~> utf8.xmap[Category](Category.apply, _.name)).upcast
      val keywordsCodec: Codec[Attribute] = (`:` ~> utf8.xmap[Keywords](Keywords.apply, _.keywords)).upcast
      val toolCodec: Codec[Attribute] = (`:` ~> utf8.xmap[Tool](Tool.apply, _.name)).upcast
      val packetTimeCodec: Codec[Attribute] = (`:` ~> msCodec.xmap[PacketTime](PacketTime.apply, _.dur)).upcast
      val maxPacketTimeCodec: Codec[Attribute] = (`:` ~> msCodec.xmap[MaxPacketTime](MaxPacketTime.apply, _.dur)).upcast
      val recvCodec: Codec[Attribute] = provide(MediaDirection(MediaDirectionType.ReceiveOnly)).upcast
      val sendRecvCodec: Codec[Attribute] = provide(MediaDirection(MediaDirectionType.SendAndReceive)).upcast
      val sendonlyCodec: Codec[Attribute] = provide(MediaDirection(MediaDirectionType.SendOnly)).upcast
      val inactiveCodec: Codec[Attribute] = provide(MediaDirection(MediaDirectionType.Inactive)).upcast
      val orientationCodec: Codec[Attribute] = (`:` ~> mappedEnum(ascii, OrientationType.values.map(v => v -> v.toString).toMap).xmap[Orientation](Orientation.apply, _.orientation)).upcast
      val conferenceTypeCodec: Codec[Attribute] = (`:` ~> utf8.xmap[ConferenceType](ConferenceType.apply, _.tpe)).upcast
      val characterSetCodec: Codec[Attribute] = (`:` ~> utf8.xmap[CharacterSet](CharacterSet.apply, _.set)).upcast
      val sdpLanguageCodec: Codec[Attribute] = (`:` ~> utf8.xmap[SDPLanguage](SDPLanguage.apply, _.language)).upcast
      val languageCodec: Codec[Attribute] = (`:` ~> utf8.xmap[Language](Language.apply, _.language)).upcast
      val frameRateCodec: Codec[Attribute] = {
        (`:` ~> listDelimited(BitVector.view(Array[Byte]('.')), intAsString).exmap[FrameRate](
          { ls =>
            if (ls.size == 1) Attempt.successful(FrameRate(ls(0), None))
            else if (ls.size == 2) Attempt.successful(FrameRate(ls(0), Some(ls(1))))
            else Attempt.failure(Err(s"Expected rate or rate.fraction, but got $ls"))
          }
          , fr => Attempt.successful(fr.frames +: fr.fraction.toList)
        )).upcast
      }
      val qualityCodec: Codec[Attribute] = (`:` ~> intAsString.xmap[Quality](Quality.apply, _.quality)).upcast
      val formatParamsCodec: Codec[Attribute] = {
        (`:` ~> ((untilWs(ascii) <~ WS) :: utf8).as[FormatParams]).upcast
      }
      val rtpMapCodec: Codec[Attribute] = {
        (`:` ~> (
          ("RTP Type" | untilWs(intAsString)) :: WS ::
          ("Encoding Name" | takeWhile(ascii)(_ != slash)) :: constant(BitVector(Array(slash))) ::
          ("Clock Rate" | takeWhile(intAsString)(_ != slash)) ::
          ("Channels" | optional(recover2(constant(BitVector(Array(slash)))), intAsString))
        ).as[RtpMap]).upcast
      }

      attributePrefix.flatZip[Attribute] {
        case "cat" => categoryCodec
        case "keywds" => keywordsCodec
        case "tool" => toolCodec
        case "ptime" => packetTimeCodec
        case "maxptime" => maxPacketTimeCodec
        case "rtpmap" => rtpMapCodec
        case "recvonly" => recvCodec
        case "sendrecv" => sendRecvCodec
        case "sendonly" => sendonlyCodec
        case "inactive" => inactiveCodec
        case "orient" => orientationCodec
        case "type" => conferenceTypeCodec
        case "charset" => characterSetCodec
        case "sdplang" => sdpLanguageCodec
        case "lang" => languageCodec
        case "framerate" => frameRateCodec
        case "quality" => qualityCodec
        case "fmtp" => formatParamsCodec
        case other => fail(Err(s"Unsupported attribute : $other"))
      }.xmap[Attribute](
        _._2
        , {
          case a: Category => "cat" -> a
          case a: Keywords => "keywds" -> a
          case a: Tool => "tool" -> a
          case a: PacketTime => "ptime" -> a
          case a: MaxPacketTime => "maxptime" -> a
          case a: RtpMap => "rtpmap" -> a
          case a: MediaDirection => a.tpe.toString -> a
          case a: Orientation => "orient" -> a
          case a: ConferenceType => "type" -> a
          case a: CharacterSet => "charset" -> a
          case a: Language => "lang" -> a
          case a: SDPLanguage => "sdplang" -> a
          case a: FrameRate => "framerate" -> a
          case a: Quality => "quality" -> a
          case a: FormatParams => "fmtp" -> a
        }
      )
    }

    // m=
    val media: Codec[MediaDescription] =  "Media Description section" | {
      val portAndCount: Codec[Int :: Option[Int] :: HNil] = {
        listDelimited(BitVector.view(Array[Byte]('/')), intAsString).exmap(
          { ints =>
            if (ints.size == 1) Attempt.successful(ints(0) :: None :: HNil)
            else if (ints.size == 2) Attempt.successful(ints(0) :: Some(ints(1)) :: HNil)
            else Attempt.failure(Err(s"Invalid format, expected port or port/count, got: $ints"))
          }
          , { case port :: maybeCount :: HNil => Attempt.successful(port +: maybeCount.toList) }
        )
      }

      val mLine: Codec[MediaType.Value :: Int :: Option[Int] :: MediaProtocol.Value :: List[Int] :: HNil] = parameterLine("Media Description",'m')((
        ("Media" | untilWs(mediaTypeEnum)) :: WS ::
        ("Port And Count" | untilWs(portAndCount)) ::: WS ::
        ("Media Protocol" | untilWs(mediaProtocolEnum)) :: WS ::
        ("Format" | listDelimited(BitVector.view(Array[Byte](' ')), intAsString))
      ).dropUnits)

      new Codec[MediaDescription] {
        def decode(bits: BitVector): Attempt[DecodeResult[MediaDescription]] = {
          @tailrec
          def go(remains: BitVector, md: MediaDescription, ln: Int): Attempt[DecodeResult[MediaDescription]] = {
            if (remains.isEmpty) Attempt.successful(DecodeResult(md, BitVector.empty))
            else if (remains.size < 16) Attempt.failure(Err.General(s"Expected ?=, but got ${remains.decodeAscii}", List(ln.toString)))
            else {
              val (decoded, continue) =
              remains.getByte(0) match {
                case 'i' => (sessionInformation.decode(remains) map { _ map { i => md.copy(information = Some(i)) } }, true)
                case 'c' => (connectionData.decode(remains) map { _ map { cd => md.copy(connectionData = md.connectionData :+ cd) } }, true)
                case 'b' => (bandwidth.decode(remains) map { _ map { bw => md.copy(bandwidth = md.bandwidth :+ bw) } }, true)
                case 'a' => (attribute.decode(remains) map { _ map { attr => md.copy(attributes = md.attributes :+ attr) } }, true)
                case 'm' => (Attempt.successful(DecodeResult(md, `\r\n` ++ remains)), false)
                case other => (Attempt.failure(Err(s"Unexpected parameter line in Media Description : '${other.toChar}'")), false)
              }

              if (!continue) decoded
              else {
                decoded match {
                  case Attempt.Successful(DecodeResult(next, rem)) =>
                    if (rem.startsWith(`\r\n`)) go(rem.drop(16), next, ln + 1)
                    else if (rem.startsWith(`\n`))  go(rem.drop(8), next, ln + 1)
                    else if (rem.isEmpty) go(rem, next, ln + 1)
                    else Attempt.Failure(Err(s"Each line of sdp must be separated by \\r\\n or \\n ($ln), ${rem.decodeUtf8 }"))
                  case Attempt.Failure(err) => Attempt.failure(err.pushContext(ln.toString))
                }
              }

            }
          }

          mLine.decode(bits).flatMap {
            case DecodeResult(mt :: port :: count :: protocol :: formats :: HNil, rem)  =>
              def next(bs: BitVector) = go(bs, MediaDescription(mt, port, count, protocol, formats, None, Nil, Nil, Nil ), 1)
              if (rem.startsWith(`\r\n`)) next(rem.drop(16))
              else if (rem.startsWith(`\n`)) next(rem.drop(8))
              else Attempt.Failure(Err(s"Each line of sdp must be separated by \\r\\n or \\n (m=), ${rem.decodeUtf8}"))

          }
        }

        def encode(md: MediaDescription): Attempt[BitVector] = {
          mLine.encode(md.tpe :: md.port :: md.portCount :: md.protocol :: md.format :: HNil)
          .nextLines(sessionInformation)(md.information.toList)
          .nextLines(connectionData)(md.connectionData)
          .nextLines(bandwidth)(md.bandwidth)
          .nextLines(attribute)(md.attributes)
        }

        def sizeBound: SizeBound = SizeBound.unknown
      }
    }

  }

}
