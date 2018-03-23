package spinoco.protocol.ldap

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.ClassTag
import spinoco.protocol.ldap.elements.LDAPOID
import spinoco.protocol.common

/**
  * Additional operations that are not already defined in protocol.
  *
  * @param requestName    The id of the operation.
  * @param requestValue   The value of the request.
  */
case class ExtendedRequest(
  requestName: LDAPOID
  , requestValue: Option[ByteVector]
) extends ProtocolOp

object ExtendedRequest {

  // Codec without the BER wrapping
  val codecInner: Codec[ExtendedRequest] = {
    (ber.codecSingle(ClassTag.Context, false, 0)(LDAPOID.codecInner) ::
     common.codec.maybe(ber.codecSingle(ClassTag.Context, false, 1)(scodec.codecs.bytes))
    ).as[ExtendedRequest]
  }

}
