package spinoco.protocol.ldap.elements

import scodec.{Attempt, Codec}
import spinoco.protocol.ldap


/**
  * The OID identification of an attribute.
  *
  * @param oid  The dotted decimal format of the object identifier.
  */
case class LDAPOID(
  oid: AttributeDescription.DottedDecimal
)

object LDAPOID {

  val codec: Codec[LDAPOID] = ldap.ldapString.narrow(decode, encode)

  // Codec without the BER wrapping
  val codecInner: Codec[LDAPOID] = ldap.ldapStringInner.narrow(decode, encode)

  def decode(str: String): Attempt[LDAPOID] = {
    AttributeDescription.decodeDottedDecimal(str).map(LDAPOID(_))
  }

  def encode(oio: LDAPOID): String = {
    oio.oid.values.mkString(".")
  }

}
