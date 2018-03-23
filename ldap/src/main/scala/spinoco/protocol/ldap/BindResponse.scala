package spinoco.protocol.ldap

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.ClassTag
import spinoco.protocol.ldap.elements.LDAPResult
import spinoco.protocol.common.codec.maybe

/**
  * The result of a bind operation.
  *
  * @param result           The way bind resulted.
  * @param serverSaslCreds  If server side SASL authentication is required, this field
  *                         contains data for it.
  */
case class BindResponse(
  result: LDAPResult
  , serverSaslCreds: Option[ByteVector]
) extends ProtocolOp

object BindResponse{

  // Codec without the BER wrapping
  val codecInner: Codec[BindResponse] =
    (LDAPResult.codecInner ::
     maybe(ber.codecSingle(ClassTag.Context, false, 7)(scodec.codecs.bytes))
    ).as[BindResponse]

}
