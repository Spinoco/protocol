package spinoco.protocol.mgcp

import shapeless.tag.@@

import scala.concurrent.duration.FiniteDuration


object MGCPParameter {

  case class ResponseAck(ack: List[AckTx]) extends MGCPParameter
  case class BearerInformation(attrs: List[BearerAttribute]) extends MGCPParameter
  case class CallId(callId: String @@ CallId) extends MGCPParameter
  case class ConnectionId(connectionId: String @@ ConnectionId) extends MGCPParameter
  case class NotifiedEntity(entity: EntityName) extends MGCPParameter
  case class RequestIdentifier(id: String @@ RequestIdentifier) extends MGCPParameter
  case class LocalConnectionOptions(options: List[LocalConnectionOption]) extends MGCPParameter
  case class ConnectionMode(mode: ConnectionModeType) extends MGCPParameter
  case class RequestedEvents(events: List[RequestedEvent]) extends MGCPParameter
  case class SignalRequests(signal: List[ParametrizedEvent]) extends MGCPParameter
  case class DigitMap(map:String) extends MGCPParameter
  case class ObservedEvents(events: List[ParametrizedEvent]) extends MGCPParameter
  case class ConnectionParameters(params: List[ConnectionParameter]) extends MGCPParameter
  case class ReasonCode(code: ReasonCodeType.Value) extends MGCPParameter
  case class SpecificEndpointID(name: LocalEndpointName) extends MGCPParameter
  case class SecondEndpointID(name: LocalEndpointName) extends MGCPParameter
  case class SecondConnectionID(connectionId: String @@ ConnectionId) extends MGCPParameter
  case class RequestedInfo(request:List[MGCPParameterInfoName.Value]) extends MGCPParameter
  case class QuarantineHandling(style:List[QuarantineHandlingStyle.Value]) extends MGCPParameter
  case class DetectEvents(events: List[ParametrizedEvent]) extends MGCPParameter
  case class RestartMethod(method: RestartMethodType.Value) extends MGCPParameter
  case class RestartDelay(delay: FiniteDuration) extends MGCPParameter
  case class Capabilities(capabilities: List[CapabilityValue]) extends MGCPParameter
  case class EventStates(events: String) extends MGCPParameter
  case class PackageList (packages: List[PackageVersion]) extends MGCPParameter
  case class MaxMGCPDatagram(size: Int) extends MGCPParameter

}


sealed trait MGCPParameter



