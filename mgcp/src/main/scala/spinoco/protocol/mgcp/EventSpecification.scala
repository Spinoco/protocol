package spinoco.protocol.mgcp

import shapeless.tag.@@
import spinoco.protocol.mgcp.MGCPParameter.ConnectionId

/**
  * Specification (name) of the event
  */
case class EventSpecification(
  event: PackageEvent
  , owner: Option[EventOwner]
)

trait PackageEvent

object EventOwner {

  case class Connection(id: String @@ ConnectionId) extends EventOwner
  case object `*` extends EventOwner
  case object `$` extends EventOwner
}

sealed trait EventOwner