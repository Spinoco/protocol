package spinoco.protocol.kafka.codec

import java.nio.ByteBuffer

import kafka.api.{FetchRequest, ProducerRequest, RequestOrResponse, TopicMetadataRequest}
import org.scalacheck.{Arbitrary, Gen}
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.common.ProtocolSpec
import spinoco.protocol.kafka.{ApiKey, ProtocolVersion}


class CodecSpec extends ProtocolSpec {


  implicit val pvGen = Arbitrary(Gen.oneOf(ProtocolVersion.values.toSeq))

  def serializeRequest(rq:RequestOrResponse):BitVector = {
    val sz = rq.sizeInBytes
    val buffer = ByteBuffer.allocate(sz)

    val apiId =
      rq match {
        case _: TopicMetadataRequest => ApiKey.MetadataRequest.id
        case _: ProducerRequest => ApiKey.ProduceRequest.id
        case _: FetchRequest => ApiKey.FetchRequest.id
      }

    rq.writeTo(buffer)
    buffer.rewind()
    (ByteVector.fromInt(sz+2) ++
      ByteVector.fromShort(apiId.toShort) ++
      ByteVector.view(buffer)).toBitVector
  }


  def serializeResponse(resp:RequestOrResponse):BitVector = {
    val sz = resp.sizeInBytes
    val buffer = ByteBuffer.allocate(sz)
    resp.writeTo(buffer)
    buffer.rewind()
    (ByteVector.fromInt(sz) ++ ByteVector.view(buffer)).toBitVector
  }


}
