package spinoco.protocol.kafka.codec

import org.apache.kafka.common.requests._
import org.scalacheck.{Arbitrary, Gen}
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.common.ProtocolSpec
import spinoco.protocol.kafka.ProtocolVersion


class CodecSpec extends ProtocolSpec {

  implicit val pvGen = Arbitrary(Gen.oneOf(ProtocolVersion.values.toSeq))

  def serializeRequest(rq: AbstractRequest): BitVector = {
    val bytes = rq.serialize(new RequestHeader(rq.api, 0, "client", 1))
    bytes.rewind()
    (ByteVector.fromInt(bytes.remaining()) ++ ByteVector.view(bytes)).toBitVector
  }


  def serializeResponse(resp: AbstractResponse, version: Short): BitVector = {
    val bytes = resp.serialize(version, new ResponseHeader(1, 0.toShort))
    bytes.rewind()
    (ByteVector.fromInt(bytes.remaining()) ++ ByteVector.view(bytes)).toBitVector
  }


}
