package spinoco.protocol.mgcp

/**
  * Created by pach on 09/03/17.
  */
case class RequestedEvent(event: EventSpecification, action: List[RequestedEventAction])



object RequestedEventAction {

  case object NotifyImmediately extends RequestedEventAction
  case object Accumulate extends RequestedEventAction
  case object TreatByDigitMap extends RequestedEventAction
  case object Swap extends RequestedEventAction
  case object Ignore extends RequestedEventAction
  case object KeepActive extends RequestedEventAction
  case class EmbeddedRequest(
    requests: List[RequestedEvent]
    , signalled: List[ParametrizedEvent]
    , digitMap: Option[String]
  ) extends RequestedEventAction

}

sealed trait RequestedEventAction
