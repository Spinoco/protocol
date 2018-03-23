package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.ClassTag
import spinoco.protocol.ldap.elements.{LDAPDN, RelativeLDAPDN}
import spinoco.protocol.common

/**
  * Request to modify the relativeDN of a given entry. This also allows to more the entry
  * and its whole sub tree to a new parent.
  *
  * @param entry          The entry that is being modified.
  * @param newRdn         The new relativeDN of the entry that is to be set.
  * @param deleteOldRdn   Whether the old relativeDN of the entry should be deleted
  * @param newSuperior    The new root for this entry and its subtree.
  */
case class ModifyDNRequest(
  entry: LDAPDN
  , newRdn: RelativeLDAPDN
  , deleteOldRdn: Boolean
  , newSuperior: Option[LDAPDN]
) extends ProtocolOp

object ModifyDNRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[ModifyDNRequest] =
    (LDAPDN.codec ::
      RelativeLDAPDN.codec ::
      boolean ::
      common.codec.maybe(ber.codecSingle(ClassTag.Context, false, 0)(LDAPDN.codecInner))
    ).as[ModifyDNRequest]


}
