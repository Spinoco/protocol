package spinoco.protocol.ldap

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.ClassTag
import spinoco.protocol.ldap.elements.{LDAPOID, LDAPResult}
import spinoco.protocol.common

/**
  * Response to [[ExtendedRequest]]
  *
  * @param response       The result of the request.
  * @param responseName   The name of the response to the request.
  * @param responseValue  The value of the response.
  */
case class ExtendedResponse(
  response: LDAPResult
  , responseName: Option[LDAPOID]
  , responseValue: Option[ByteVector]
) extends ProtocolOp

object ExtendedResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[ExtendedResponse] =
    (LDAPResult.codecInner ::
     common.codec.maybe(ber.codecSingle(ClassTag.Context, false, 10)(LDAPOID.codecInner)) ::
     common.codec.maybe(ber.codecSingle(ClassTag.Context, false, 11)(scodec.codecs.bytes))
    ).as[ExtendedResponse]

}

