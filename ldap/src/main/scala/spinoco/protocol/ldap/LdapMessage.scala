package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.BerClass
import spinoco.protocol.common.codec.maybe
import spinoco.protocol.ldap.elements.Control


/**
  * The base of all LDAP messages.
  *
  * @param messageId    The id of the message, this is only incremented by client.
  * @param protocolOp   The LDAP operation to be performed. This includes responses from server to client.
  * @param controls     The possible additional arguments for the protocol op.
  */
case class LdapMessage(
  messageId: Int
  , protocolOp: ProtocolOp
  , controls: Option[Vector[Control]]
)

object LdapMessage {

  lazy val codec: Codec[LdapMessage] =
    ber.sequence((
      ber.integer ::
      ProtocolOp.codec ::
      maybe(ber.codecSingle(BerClass.Context, true, 0)(scodec.codecs.vector(Control.codec)))
    ).as[LdapMessage])

}