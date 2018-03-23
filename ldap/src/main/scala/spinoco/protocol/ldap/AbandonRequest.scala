package spinoco.protocol.ldap

import scodec.Codec
import shapeless.tag
import shapeless.tag.@@

/**
  * Client requests a given message abandoned.
  *
  * The server as such may abandon the operation connected to the given message.
  *
  * Server MUST not send any additional data if the AbandonRequest is to as SearchRequest. That
  * includes the [[SearchResultDone]].
  *
  * @param msgId  The id of the message that should have its progress cancelled.
  */
case class AbandonRequest(
  msgId: Int @@ LdapMessage
) extends ProtocolOp

object AbandonRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[AbandonRequest] =
    scodec.codecs.vint.xmap(msgId => AbandonRequest(tag[LdapMessage](msgId)), _.msgId)

}
