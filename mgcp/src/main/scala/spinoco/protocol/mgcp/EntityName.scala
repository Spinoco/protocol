package spinoco.protocol.mgcp


case class EntityName(localName: Option[LocalEndpointName], domainName: String, port: Option[Int])
