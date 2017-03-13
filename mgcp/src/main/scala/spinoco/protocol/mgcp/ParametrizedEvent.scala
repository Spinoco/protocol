package spinoco.protocol.mgcp


case class ParametrizedEvent(
  specification: EventSpecification
  , param: Option[String]
)
