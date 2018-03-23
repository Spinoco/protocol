package spinoco.protocol.ldap.elements

import scodec.{Attempt, Codec}
import spinoco.protocol.ldap


/**
  * The OID identification of an attribute.
  *
  * @param oid  The dotted decimal format of the object identifier.
  */
case class LdapOID(
  oid: AttributeDescription.DottedDecimal
)

object LdapOID {

  val codec: Codec[LdapOID] = ldap.ldapString.narrow(decode, encode)

  // Codec without the BER wrapping
  val codecInner: Codec[LdapOID] = ldap.ldapStringInner.narrow(decode, encode)

  def decode(str: String): Attempt[LdapOID] = {
    AttributeDescription.decodeDottedDecimal(str).map(LdapOID(_))
  }

  def encode(oio: LdapOID): String = {
    oio.oid.values.mkString(".")
  }

}
