package spinoco.protocol.kafka

/**
  * Reponse from kafka broker
  * @param correlationId    Correlation id as presented in request
  * @param response         Response received from kafka
  */
case class ResponseMessage (
  correlationId: Int
  , response: Response
)
