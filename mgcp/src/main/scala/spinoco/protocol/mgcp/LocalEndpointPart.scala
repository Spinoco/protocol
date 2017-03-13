package spinoco.protocol.mgcp

/**
  * Name of the enpoint
  * @param start  Initial name
  * @param parts  parts separated by slash (/)
  */
case class LocalEndpointName(start: LocalEndpointPart, parts: List[LocalEndpointPart])

object LocalEndpointPart {

  case object `$` extends LocalEndpointPart
  case object `*` extends LocalEndpointPart
  case class NameString(name: String) extends LocalEndpointPart

}


sealed trait  LocalEndpointPart


