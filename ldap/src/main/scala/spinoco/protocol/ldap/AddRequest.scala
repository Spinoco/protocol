package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.ldap.elements.{Attribute, LDAPDN}

/**
  * Adds an entry with a given attributes.
  *
  * @param entry        The unique identifier of this entity.
  * @param attributes   The attributes this entity should take
  *
  */
case class AddRequest(
  entry: LDAPDN
  , attributes: Vector[Attribute]
) extends ProtocolOp

object AddRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[AddRequest] =
    (LDAPDN.codec :: ber.sequence(scodec.codecs.vector(Attribute.codec))).as[AddRequest]

}
