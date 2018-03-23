package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.ldap.ModifyRequest.Change
import spinoco.protocol.ldap.elements.{LdapDN, PartialAttribute}

/**
  * A request to modify given entry with given changes.
  *
  * @param entry    The entry to be modified.
  * @param changes  The changes to be applied to the entry.
  */
case class ModifyRequest(
  entry: LdapDN
  , changes: Vector[Change]
) extends ProtocolOp

object ModifyRequest {

  case class Change(
    operation: ModifyOperation.Value
    , modification: PartialAttribute
  )

  object ModifyOperation extends Enumeration {
    val add = Value(0)
    val delete = Value(1)
    val replace = Value(2)
  }

  val changeCodec: Codec[Change] =
    ber.sequence((
      scodec.codecs.enumerated(ber.enumerated, ModifyOperation) ::
      PartialAttribute.codec
    ).as[Change])

  // Codec without the BER wrapping
  val codecInner: Codec[ModifyRequest] =
    (LdapDN.codec :: ber.sequence(scodec.codecs.vector(changeCodec))).as[ModifyRequest]

}
