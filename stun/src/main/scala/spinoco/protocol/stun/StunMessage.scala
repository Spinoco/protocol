package spinoco.protocol.stun




/**
  *
  *
  * Represent Stun message as defined in rfc5389 (https://tools.ietf.org/html/rfc5389#section-6)
  *
  *
  * @param messageClass       The message type defines the message class (request, success
  *                           response, failure response, or indication) and the message method
  *                           (the primary function) of the STUN message.  Although there are four
  *                           message classes, there are only two types of transactions in STUN:
  *                           request/response transactions (which consist of a request message and
  *                           a response message) and indication transactions (which consist of a
  *                           single indication message).  Response classes are split into error
  *                           and success responses to aid in quickly processing the STUN message.
  *
  * @param method             Method of the stun message. Currently only Binding method is defined
  *
  * @param transactionId      The transaction ID is a 96-bit identifier, used to uniquely identify
  *                           STUN transactions.  For request/response transactions, the
  *                           transaction ID is chosen by the STUN client for the request and
  *                           echoed by the server in the response.  For indications, it is chosen
  *                           by the agent sending the indication.  It primarily serves to
  *                           correlate requests with responses, though it also plays a small role
  *                           in helping to prevent certain types of attacks.  The server also uses
  *                           the transaction ID as a key to identify each transaction uniquely
  *                           across all clients.  As such, the transaction ID MUST be uniformly
  *                           and randomly chosen from the interval 0 .. 2**96-1, and SHOULD be
  *                           cryptographically random.  Resends of the same request reuse the same
  *                           transaction ID, but the client MUST choose a new transaction ID for
  *                           new transactions unless the new request is bit-wise identical to the
  *                           previous request and sent from the same transport address to the same
  *                           IP address.  Success and error responses MUST carry the same
  *                           transaction ID as their corresponding request.  When an agent is
  *                           acting as a STUN server and STUN client on the same port, the
  *                           transaction IDs in requests sent by the agent have no relationship to
  *                           the transaction IDs in requests received by the agent.
  *
  * @param attribute          List of STUN attributes contained in the message
  */
case class StunMessage(
  messageClass: MessageClass
  , method: StunMethod
  , transactionId: TransactionId
  , attribute: Vector[StunAttribute]
)


object StunMessage {



}



sealed trait MessageClass

object MessageClass {

  case object Request extends MessageClass

  case object Indication extends MessageClass

  case class Response(success:Boolean ) extends MessageClass

}



sealed trait StunMethod


object StunMethod {

  case object Binding extends StunMethod

}

