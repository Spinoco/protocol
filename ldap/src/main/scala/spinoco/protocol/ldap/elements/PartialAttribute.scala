package spinoco.protocol.ldap.elements

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.ldap._
import spinoco.protocol.common

/**
  * Same as [[Attribute]] but does not have to have any value provided to it.
  *
  * @param tpe  The type of the attribute.
  * @param vals The values for the given attribute type.
  */
case class PartialAttribute(
  tpe: AttributeDescription
  , vals: Set[AttributeValue]
)

object PartialAttribute {

  val codec: Codec[PartialAttribute] =
    ber.sequence((
      AttributeDescription.codec ::
      ber.set(common.codec.set(attributeValue))
    ).as[PartialAttribute])

  type PartialAttributeList = Vector[PartialAttribute]
  val listCodec: Codec[PartialAttributeList] =
    ber.sequence(scodec.codecs.vector(codec))

}
