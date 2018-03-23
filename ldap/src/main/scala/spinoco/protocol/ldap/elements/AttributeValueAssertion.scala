package spinoco.protocol.ldap.elements

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.ldap.{AssertionValue, assertionValue}

/**
  * Assertion of whether a given attribute matches the given value for said attribute.
  *
  * @param attributeDesc    The identification of the attribute.
  * @param assertionValue   The value of the attribute against which we are matching.
  */
case class AttributeValueAssertion(
  attributeDesc: AttributeDescription
  , assertionValue: AssertionValue
)

object AttributeValueAssertion {

  // Codec without the BER wrapping
  val codecInner: Codec[AttributeValueAssertion] =
    (AttributeDescription.codec ::
      assertionValue
      ).as[AttributeValueAssertion]

  val codec: Codec[AttributeValueAssertion] =
    ber.sequence(codecInner)

}