package spinoco.protocol.ldap

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.BerClass
import spinoco.protocol.ldap.elements.LdapOID
import spinoco.protocol.common

/**
  * Additional operations that are not already defined in protocol.
  *
  * @param requestName    The id of the operation.
  * @param requestValue   The value of the request.
  */
case class ExtendedRequest(
  requestName: LdapOID
  , requestValue: Option[ByteVector]
) extends ProtocolOp

object ExtendedRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[ExtendedRequest] = {
    (ber.codecSingle(BerClass.Context, false, 0)(LdapOID.codecInner) ::
     common.codec.maybe(ber.codecSingle(BerClass.Context, false, 1)(scodec.codecs.bytes))
    ).as[ExtendedRequest]
  }

}
