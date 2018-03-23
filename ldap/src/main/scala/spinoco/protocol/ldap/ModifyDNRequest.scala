package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.BerClass
import spinoco.protocol.ldap.elements.{LdapDN, RelativeLdapDN}
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
  entry: LdapDN
  , newRdn: RelativeLdapDN
  , deleteOldRdn: Boolean
  , newSuperior: Option[LdapDN]
) extends ProtocolOp

object ModifyDNRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[ModifyDNRequest] =
    (LdapDN.codec ::
      RelativeLdapDN.codec ::
      boolean ::
      common.codec.maybe(ber.codecSingle(BerClass.Context, false, 0)(LdapDN.codecInner))
    ).as[ModifyDNRequest]


}
