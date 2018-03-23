package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LdapDN

/**
  * Request to delete a given entry.
  *
  * @param entry  The entry to be deleted.
  */
case class DelRequest(
  entry: LdapDN
) extends ProtocolOp

object DelRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[DelRequest] =
    LdapDN.codecInner.xmap(DelRequest(_), _.entry)

}
