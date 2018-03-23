package spinoco.protocol.ldap

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.ClassTag
import spinoco.protocol.common
import spinoco.protocol.ldap.elements.LDAPOID

/**
  * A mechanism for a response to a client before the request is completed.
  *
  * This is like [[SearchResultEntry]] but for requests other than [[SearchRequest]]
  *
  * @param responseName   The name of the response.
  * @param responseValue  The value of the response.
  */
case class IntermediateResponse(
  responseName: Option[LDAPOID]
  , responseValue: Option[ByteVector]
) extends ProtocolOp

object IntermediateResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[IntermediateResponse] =
    (common.codec.maybe(ber.codecSingle(ClassTag.Context, false, 10)(LDAPOID.codecInner)) ::
      common.codec.maybe(ber.codecSingle(ClassTag.Context, false, 11)(scodec.codecs.bytes))
    ).as[IntermediateResponse]

}

