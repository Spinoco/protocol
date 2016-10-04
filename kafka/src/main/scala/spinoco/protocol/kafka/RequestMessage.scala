package spinoco.protocol.kafka

/**
  * Request to kafka
  *
  * @param version          Version of the broker
  * @param correlationId    User specified correlation id to match request/response
  * @param clientId         Id of the client for debugging purposes
  * @param request          Actual request to make
  */
case class RequestMessage(
  version:ProtocolVersion.Value
  , correlationId:Int
  , clientId: String
  , request: Request
)
