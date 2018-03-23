package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.{ClassTag, Identifier}

trait ProtocolOp

object ProtocolOp {

  lazy val codec: Codec[ProtocolOp] =
    ber.discriminated[ProtocolOp]
    .typecase(Identifier(ClassTag.Application, true, 0), ber.finiteLength(BindRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 1), ber.finiteLength(BindResponse.codecInner))
    .typecase(Identifier(ClassTag.Application, false, 2), ber.finiteLength(UnbindRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 3), ber.finiteLength(SearchRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 4), ber.finiteLength(SearchResultEntry.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 5), ber.finiteLength(SearchResultDone.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 6), ber.finiteLength(ModifyRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 7), ber.finiteLength(ModifyResponse.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 8), ber.finiteLength(AddRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 9), ber.finiteLength(AddResponse.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 10), ber.finiteLength(DelRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 11), ber.finiteLength(DelResponse.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 12), ber.finiteLength(ModifyDNRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 13), ber.finiteLength(ModifyDNResponse.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 14), ber.finiteLength(CompareRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 15), ber.finiteLength(CompareResponse.codecInner))
    .typecase(Identifier(ClassTag.Application, false, 16), ber.finiteLength(AbandonRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 23), ber.finiteLength(ExtendedRequest.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 24), ber.finiteLength(ExtendedResponse.codecInner))
    .typecase(Identifier(ClassTag.Application, true, 25), ber.finiteLength(IntermediateResponse.codecInner))

}
