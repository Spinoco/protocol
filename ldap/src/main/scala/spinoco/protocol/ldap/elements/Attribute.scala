package spinoco.protocol.ldap.elements

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.ldap.{AttributeValue, attributeValue}
import spinoco.protocol.common.codec._

/**
  * An attribute with its description and at least one value.
  *
  * @param tpe  The type of the attribute.
  * @param vals The values for the given attribute type. This set of values have
  *             to have at least one value.
  */
case class Attribute(
  tpe: AttributeDescription
  , vals: Set[AttributeValue]
)

object Attribute {

  val codec: Codec[Attribute] =
    ber.sequence((
      AttributeDescription.codec ::
      ber.set(minItems(1)(set(attributeValue)))
    ).as[Attribute])

}