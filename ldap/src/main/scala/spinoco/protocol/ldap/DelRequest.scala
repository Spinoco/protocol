package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LDAPDN

/**
  * Request to delete a given entry.
  *
  * @param entry  The entry to be deleted.
  */
case class DelRequest(
  entry: LDAPDN
) extends ProtocolOp

object DelRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[DelRequest] =
    LDAPDN.codecInner.xmap(DelRequest(_), _.entry)

}
