package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.{AttributeValueAssertion, LDAPDN}

/**
  * A request to compare a given attribute of a given entry with the provided value.
  *
  * @param entry  The entry which is to be compared.
  * @param ava    The attribute that is to be compared
  */
case class CompareRequest(
  entry: LDAPDN
  , ava: AttributeValueAssertion
) extends ProtocolOp

object CompareRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[CompareRequest] =
    (LDAPDN.codec :: AttributeValueAssertion.codec).as[CompareRequest]

}
