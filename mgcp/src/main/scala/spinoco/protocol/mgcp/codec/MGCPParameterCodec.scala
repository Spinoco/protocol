package spinoco.protocol.mgcp.codec

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector
import scodec.codecs._
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}
import spinoco.protocol.mgcp._
import spinoco.protocol.mgcp.MGCPParameter._
import spinoco.protocol.common.codec._
import spinoco.protocol.mgcp.BearerAttribute.{BearerEncoding, BearerExtensionName}
import spinoco.protocol.mgcp.CapabilityValue.{SupportedModes, SupportedPackages}
import spinoco.protocol.mgcp.LocalConnectionOption._

import scala.annotation.tailrec
import scala.concurrent.duration._


object MGCPParameterCodec {


  /**
    * A combinator, that first splits the stream by comma and then applies codec, stripping any whitespace after coma
    */
  def delimitedByComma[A](codec: Codec[A]):Codec[List[A]] = {
    listDelimited(BitVector.view(",".getBytes), takeWhileChar(dropWS ~> codec)(','))
  }

  /**
    * Like `delimitedByComma`, but codec is first applied, and then if there is another item (because of the next character being comma
    * then next codec is applied
    */
  def delimitedByComma2[A](codec: Codec[A]): Codec[List[A]] = {
    val commaSpace =  BitVector.view(", ".getBytes())
    val comma = ','.toByte
    @tailrec
    def go(acc: Vector[A], bs: BitVector): Attempt[DecodeResult[List[A]]] = {
      codec.decode(bs) match {
        case Attempt.Failure(err) => Attempt.failure(err)
        case Attempt.Successful(DecodeResult(a, rem)) =>

          if (rem.isEmpty) Attempt.successful(DecodeResult((acc :+ a).toList, BitVector.empty))
          else if (rem.getByte(0) != comma) Attempt.failure(Err("Separator must be `,` optionally followed by whitespace"))
          else {
            val next = rem.bytes.tail.dropWhile(_.toChar.isWhitespace)
            go(acc :+ a, next.bits)
          }

      }
    }

    new Codec[List[A]] {
      val encoder = listDelimited(commaSpace, codec)
      def sizeBound: SizeBound = SizeBound.unknown
      def encode(value: List[A]): Attempt[BitVector] = encoder.encode(value)
      def decode(bits: BitVector): Attempt[DecodeResult[List[A]]] = go(Vector.empty, bits)

    }
  }


  val responseAckCodec: Codec[ResponseAck] =  {
     val ackTxCodec: Codec[AckTx] =
       untilWs(transactionId :: optional(recover2(constant(BitVector.view("-".getBytes))), transactionId))
       .as[AckTx]

     delimitedByComma(ackTxCodec)
     .as[ResponseAck]
  }


  val bearerInformationCodec: Codec[BearerInformation] = {
    val baCodec: Codec[BearerAttribute] = {
      choice(
        (constant(BitVector("e:".getBytes)) :: mappedEnum(ascii, BearerEncodingType.values.map { v => v -> v.toString}.toMap)).as[BearerEncoding].upcast
        , ascii.as[BearerExtensionName].upcast
      )
    }

    delimitedByComma(baCodec)
    .as[BearerInformation]
  }




  val callIdCodec: Codec[CallId] = {
    tagged[String, CallId](guard(ascii)(is32Hex("CallId")))
    .as[CallId]
  }

  val connectionIdParameterCodec: Codec[ConnectionId] = {
    connectionIdCodec.as[ConnectionId]
  }

  val notifiedEntityCodec: Codec[NotifiedEntity] = {
    val entityNameCodec: Codec[EntityName] = {

      val withLocalPart =
        (
          ("LocalName" | takeWhileChar(localEndpointNameCodec)('@') <~ constantString("@"))
            .xmap[Some[LocalEndpointName]](Some(_), _.get).upcast[Option[LocalEndpointName]] ::
            ("Domain" | takeWhileChar(domainCodec)(':')) ::
            ("Port" | optional(recover2(constant(BitVector(":".getBytes))), intAsString))
          ).as[EntityName]

      val domainOnly =
        (
          ("LocalName" | provide(None).upcast[Option[LocalEndpointName]]) ::
          ("Domain" | takeWhileChar(domainCodec)(':')) ::
          ("Port" | optional(recover2(constant(BitVector(":".getBytes))), intAsString))
        ).as[EntityName]

      choice(
        withLocalPart
        , domainOnly
      )
    }

    entityNameCodec
    .as[NotifiedEntity]
  }

  val requestIdentifierCodec: Codec[RequestIdentifier] = {
    tagged[String, RequestIdentifier](guard(ascii)(is32Hex("RequestIdentifier")))
    .as[RequestIdentifier]
  }

  val localOptionCodec: Codec[LocalConnectionOption] = {
    val compressionAlgorithmCodec: Codec[CompressionAlgorithm] =
      ascii.as[CompressionAlgorithm]

    val bandwidthCodec: Codec[Bandwidth] =
      (intAsString :: optional(recover2(constant(BitVector.view("-".getBytes))), intAsString)).as[Bandwidth]

    val echoCancelCodec: Codec[EchoCancel] =
      mappedEnum(ascii, Map(true -> "on", false -> "off" ))
        .as[EchoCancel]

    val packetizationPeriodCodec: Codec[PacketizationPeriod] =
      intAsString.as[PacketizationPeriod]

    val gainControlCodec: Codec[GainControl] =
      choice[Option[Int]](
        constant(BitVector.view("auto".getBytes)).xmap[None.type](_ => None, _ => ()).upcast
        , intAsString.xmap[Some[Int]](Some(_), _.get).upcast
      ).as[GainControl]

    val silenceSuppressionCodec: Codec[SilenceSuppression] =
      mappedEnum(ascii, Map(true -> "on", false -> "off" ))
        .as[SilenceSuppression]


    val typeOfServiceCodec: Codec[TypeOfService] =
      ascii.as[TypeOfService]

    val resourceReservationCodec: Codec[ResourceReservation] =
      ascii.as[ResourceReservation]

    val typeOfNetworkCodec: Codec[TypeOfNetwork] = {
      val ntCodec = mappedEnum(ascii, NetworkType.values.map { v => v -> v.toString }.toMap)

      listDelimited(BitVector.view(";".getBytes), ntCodec).narrow(
        ls => {
          ls.headOption match {
            case None => Attempt.failure(Err("At least one type of the netowrk must be specified"))
            case Some(nt) => Attempt.successful(TypeOfNetwork(nt, ls.tail))
          }
        }
        , ton => ton.nt +: ton.supported
      )
    }


    val keyCodec: Codec[String] = takeWhile(ascii)(_ != ':'.asInstanceOf[Byte]) <~ constant(BitVector.view(":".getBytes))

    choice(
      discriminated[LocalConnectionOption].by(keyCodec)
        .typecase("p", packetizationPeriodCodec)
        .typecase("a", compressionAlgorithmCodec)
        .typecase("b", bandwidthCodec)
        .typecase("e", echoCancelCodec)
        .typecase("gc", gainControlCodec)
        .typecase("s", silenceSuppressionCodec)
        .typecase("t", typeOfServiceCodec)
        .typecase("r", resourceReservationCodec)
        .typecase("nt", typeOfNetworkCodec)
      , (keyCodec :: utf8).as[VendorOption].upcast[LocalConnectionOption]
    )
  }

  val localConnectionOptionsCodec: Codec[LocalConnectionOptions] = {
    delimitedByComma(localOptionCodec).as[LocalConnectionOptions]
  }

  val connectionModeTypeCodec: Codec[ConnectionModeType] = {
    import ConnectionModeType._
    choice(
      mappedEnum(ascii, Map(
        SendOnly -> "sendonly"
        , ReceiveOnly -> "recvonly"
        , SendAndReceive -> "sendrecv"
        , Inactive -> "inactive"
        , Conference -> "confrnce"
        , Loopback -> "loopback"
        , ConnectionTest -> "conttest"
        , NetworkLoop -> "netwloop"
        , NetworkTest -> "netwtest"
      ))
      , (takeWhile(ascii)(_ != '/'.asInstanceOf[Byte]) :: constant(BitVector.view("/".getBytes)) :: utf8).as[Extension].upcast[ConnectionModeType]
    )
  }

  val connectionModeCodec: Codec[ConnectionMode] =
    connectionModeTypeCodec.as[ConnectionMode]



  val listOfRequestedEventsCodec: Codec[List[RequestedEvent]] = {
    import RequestedEventAction._
    val embeddedEventActionCodec: Codec[EmbeddedRequest] = {
      val RCodec: Codec[List[RequestedEvent]] =
        constantString("R") ~> guard(enclosedBy('(',')')(lazily(listOfRequestedEventsCodec))) {
          ls => if (ls.isEmpty) Some(Err("Requested Events must not be empty")) else None
        }
      val SCodec: Codec[List[ParametrizedEvent]] =
        constantString("S") ~> guard(enclosedBy('(',')')(lazily(listOfParametrizedEvents))) {
          ls => if (ls.isEmpty) Some(Err("Signalled Events must not be empty")) else None
        }
      val DCodec: Codec[String] = constantString("D") ~> enclosedBy('(',')')(ascii)

      type Choice = List[RequestedEvent] :+: List[ParametrizedEvent] :+: String :+: CNil


      constantString("E") ~> enclosedBy('(',')')(
        delimitedByComma2((RCodec :+: SCodec :+: DCodec).choice)
      ).xmap (
        ls => {
          ls.foldLeft(EmbeddedRequest(Nil, Nil, None)) {
            case (er, Inr(Inr(Inl(digitmap)))) => er.copy(digitMap = Some(digitmap))
            case (er, Inr(Inl(signalled))) => er.copy(signalled = signalled)
            case (er, Inl(requests)) => er.copy(requests = requests)
            case (er, _) => er
          }
        }
        , er => {
          er.requests.headOption.toList.map { _ => Coproduct[Choice](er.requests) } ++
          er.signalled.headOption.toList.map { _ => Coproduct[Choice](er.signalled) } ++
          er.digitMap.toList.map(Coproduct[Choice](_))
        }
      )
    }

    val actionCodec = "Event Action" |
      choice[RequestedEventAction](
        constantString("N").decodeAs(NotifyImmediately).upcast
        , constantString("A").decodeAs(Accumulate).upcast
        , constantString("D").decodeAs(TreatByDigitMap).upcast
        , constantString("S").decodeAs(Swap).upcast
        , constantString("I").decodeAs(Ignore).upcast
        , constantString("K").decodeAs(KeepActive).upcast
        , "Embeded  " | embeddedEventActionCodec.upcast
      )
    val eventCodec: Codec[RequestedEvent] = {
      (
        takeWhileChar(eventSpecificationCodec)('(', ',') ::
        ("Event Actions" | optional(lookahead2(constantString("(")), enclosedBy('(', ')')(delimitedByComma2(actionCodec))))
        .xmap[List[RequestedEventAction]](_.toList.flatten, ls => ls.headOption.map(_ => ls))
      ).as[RequestedEvent]
    }

    delimitedByComma2(eventCodec)
  }

  val requestedEventsCodec: Codec[RequestedEvents] = {
    listOfRequestedEventsCodec.as[RequestedEvents]
  }

  val listOfParametrizedEvents: Codec[List[ParametrizedEvent]]  = {

    val eventCodec: Codec[ParametrizedEvent] = {
      (eventSpecificationCodec ::
        ("Actions" | optional(recover2(constantString("(")), takeWhile(ascii)(_ != ')'.asInstanceOf[Byte]) <~ constantString(")") ))
        )
        .as[ParametrizedEvent]
    }
    delimitedByComma(eventCodec)
  }

  val signalRequestsCodec: Codec[SignalRequests] = {
    listOfParametrizedEvents.as[SignalRequests]
  }

  val digitMapCodec: Codec[DigitMap] =
    utf8.as[DigitMap]

  val observedEventsCodec: Codec[ObservedEvents] =
    listOfParametrizedEvents.as[ObservedEvents]

  val connectionParametersCodec: Codec[ConnectionParameters] = {
    import ConnectionParameter._
    val keyCodec: Codec[String] = takeWhile(ascii)(_ != '='.asInstanceOf[Byte])
    val param =
      discriminated[ConnectionParameter].by(keyCodec)
      .typecase("PS", intAsString.as[PacketSent])
      .typecase("OS", intAsString.as[OctetsSent])
      .typecase("PR", intAsString.as[PacketReceived])
      .typecase("OR", intAsString.as[OctetsReceived])
      .typecase("PL", intAsString.as[PacketLost])
      .typecase("JI", intAsString.as[Jitter])
      .typecase("LA", intAsString.as[AvgLatency])

    delimitedByComma(param).as[ConnectionParameters]
  }


  val reasonCodeCodec: Codec[ReasonCode] = {
    stringEnumerated(ascii, ReasonCodeType).as[ReasonCode]
  }

  val specificEndpointIDCodec: Codec[SpecificEndpointID] =
    localEndpointNameCodec.as[SpecificEndpointID]

  val secondEndpointIDCodec: Codec[SecondEndpointID] =
    localEndpointNameCodec.as[SecondEndpointID]

  val secondConnectionIDCodec: Codec[SecondConnectionID] =
    tagged[String, ConnectionId](guard(ascii)(is32Hex("SecondConnectionID")))
      .as[SecondConnectionID]

  val requestedInfoCodec: Codec[RequestedInfo] =
    delimitedByComma(
      stringEnumerated(ascii, MGCPParameterInfoName)
    ).as[RequestedInfo]

  val quarantineHandlingCodec: Codec[QuarantineHandling] =
    delimitedByComma(
      stringEnumerated(ascii, QuarantineHandlingStyle)
    ).as[QuarantineHandling]

  val detectEventsCodec: Codec[DetectEvents] =
    listOfParametrizedEvents.as[DetectEvents]

  val restartMethodCodec: Codec[RestartMethod] =
    stringEnumerated(ascii, RestartMethodType).as[RestartMethod]

  val restartDelayCodec: Codec[RestartDelay] =
    intAsString.xmap[FiniteDuration](_.seconds, _.toSeconds.toInt).as[RestartDelay]

  val capabilitiesCodec: Codec[Capabilities] = {
    val supportedPackagesCodec : Codec[SupportedPackages] =
      constantString("v:") ~>
      listDelimited(BitVector.view(";".getBytes), utf8).as[SupportedPackages]
    val supportedModesCodec: Codec[SupportedModes] =
      constantString("m:") ~>
      listDelimited(BitVector.view(";".getBytes), connectionModeTypeCodec).as[SupportedModes]

    val valueCodec =
      choice[CapabilityValue](
        localOptionCodec.upcast
        , supportedPackagesCodec.upcast
        , supportedModesCodec.upcast
      )
    delimitedByComma(valueCodec).as[Capabilities]
  }

  val eventStatesCodec: Codec[EventStates] =
    utf8.as[EventStates]

  val packageListCodec: Codec[PackageList] = {
    val packageVersionCodec: Codec[PackageVersion] = {
      (takeWhile(utf8)(_ != ':'.asInstanceOf[Byte]) :: constantString(":") ::
      ascii).as[PackageVersion]
    }
    listDelimited(BitVector.view(",".getBytes), packageVersionCodec).as[PackageList]
  }

  val maxMGCPDatagramCodec: Codec[MaxMGCPDatagram] =
    intAsString.as[MaxMGCPDatagram]


  val codec: Codec[MGCPParameter] = {
    val keyCodec: Codec[String] = takeWhile(ascii)(_ != ':'.asInstanceOf[Byte]) <~ constant(BitVector.view(":".getBytes())) <~ WS

    val keyAndParamCodec =
    discriminated[MGCPParameter].by(keyCodec)
    .typecase("K", "ResponseAck" | responseAckCodec)
    .typecase("B", "BearerInformation" | bearerInformationCodec)
    .typecase("C", "CallId" | callIdCodec)
    .typecase("I", "ConnectionId" | connectionIdParameterCodec)
    .typecase("N", "NotifiedEntity" | notifiedEntityCodec)
    .typecase("X", "RequestIdentifier" | requestIdentifierCodec)
    .typecase("L", "LocalConnectionOptions" | localConnectionOptionsCodec)
    .typecase("M", "ConnectionMode" | connectionModeCodec)
    .typecase("R", "RequestEvents" | requestedEventsCodec)
    .typecase("S", "SignalRequests" | signalRequestsCodec)
    .typecase("D", "DigitMap" | digitMapCodec)
    .typecase("O", "ObservedEvents" | observedEventsCodec)
    .typecase("P", "ConnectionParameter" | connectionParametersCodec)
    .typecase("E", "ReasonCode" | reasonCodeCodec)
    .typecase("Z", "SpecificEndpointId" | specificEndpointIDCodec)
    .typecase("Z2", "SecondEnpointId" | secondEndpointIDCodec)
    .typecase("I2", "SecondConnectionId" | secondConnectionIDCodec)
    .typecase("F", "RequestedInfo" | requestedInfoCodec)
    .typecase("Q", "QuarantineHandling" | quarantineHandlingCodec)
    .typecase("T", "DetectEvents" | detectEventsCodec)
    .typecase("RM", "RestartMethod" | restartMethodCodec)
    .typecase("RD", "RestartDelay" | restartDelayCodec)
    .typecase("A", "Capabilities" | capabilitiesCodec)
    .typecase("ES", "EventStates" | eventStatesCodec)
    .typecase("PL", "PackageList" | packageListCodec)
    .typecase("MD", "MaxMGCPDatagram" | maxMGCPDatagramCodec)

    untilEOL(keyAndParamCodec)
  }



}
