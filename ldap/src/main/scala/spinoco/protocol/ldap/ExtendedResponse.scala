package spinoco.protocol.ldap

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.BerClass
import spinoco.protocol.ldap.elements.{LdapOID, LdapResult}
import spinoco.protocol.common

/**
  * Response to [[ExtendedRequest]]
  *
  * @param response       The result of the request.
  * @param responseName   The name of the response to the request.
  * @param responseValue  The value of the response.
  */
case class ExtendedResponse(
  response: LdapResult
  , responseName: Option[LdapOID]
  , responseValue: Option[ByteVector]
) extends ProtocolOp

object ExtendedResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[ExtendedResponse] =
    (LdapResult.codecInner ::
     common.codec.maybe(ber.codecSingle(BerClass.Context, false, 10)(LdapOID.codecInner)) ::
     common.codec.maybe(ber.codecSingle(BerClass.Context, false, 11)(scodec.codecs.bytes))
    ).as[ExtendedResponse]

}

