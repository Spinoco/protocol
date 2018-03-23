package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.{BerClass, Identifier}

trait ProtocolOp

object ProtocolOp {

  lazy val codec: Codec[ProtocolOp] =
    ber.discriminated[ProtocolOp]
    .typecase(Identifier(BerClass.Application, true, 0), ber.finiteLength(BindRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 1), ber.finiteLength(BindResponse.codecInner))
    .typecase(Identifier(BerClass.Application, false, 2), ber.finiteLength(UnbindRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 3), ber.finiteLength(SearchRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 4), ber.finiteLength(SearchResultEntry.codecInner))
    .typecase(Identifier(BerClass.Application, true, 5), ber.finiteLength(SearchResultDone.codecInner))
    .typecase(Identifier(BerClass.Application, true, 6), ber.finiteLength(ModifyRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 7), ber.finiteLength(ModifyResponse.codecInner))
    .typecase(Identifier(BerClass.Application, true, 8), ber.finiteLength(AddRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 9), ber.finiteLength(AddResponse.codecInner))
    .typecase(Identifier(BerClass.Application, true, 10), ber.finiteLength(DelRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 11), ber.finiteLength(DelResponse.codecInner))
    .typecase(Identifier(BerClass.Application, true, 12), ber.finiteLength(ModifyDNRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 13), ber.finiteLength(ModifyDNResponse.codecInner))
    .typecase(Identifier(BerClass.Application, true, 14), ber.finiteLength(CompareRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 15), ber.finiteLength(CompareResponse.codecInner))
    .typecase(Identifier(BerClass.Application, false, 16), ber.finiteLength(AbandonRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 23), ber.finiteLength(ExtendedRequest.codecInner))
    .typecase(Identifier(BerClass.Application, true, 24), ber.finiteLength(ExtendedResponse.codecInner))
    .typecase(Identifier(BerClass.Application, true, 25), ber.finiteLength(IntermediateResponse.codecInner))

}
