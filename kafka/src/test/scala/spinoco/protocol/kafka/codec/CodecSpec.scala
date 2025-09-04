package spinoco.protocol.kafka.codec

import java.nio.{Buffer, ByteBuffer}

import kafka.api._
import org.scalacheck.{Arbitrary, Gen}
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.common.ProtocolSpec
import spinoco.protocol.kafka.{ApiKey, ProtocolVersion}


class CodecSpec extends ProtocolSpec {


  implicit val pvGen = Arbitrary(Gen.oneOf(ProtocolVersion.values.toSeq))

  def serializeRequest(rq:RequestOrResponse): BitVector = {
    val sz = rq.sizeInBytes
    val buffer = ByteBuffer.allocate(sz)

    val apiId =
      rq match {
        case _: TopicMetadataRequest => ApiKey.MetadataRequest.id
        case _: ProducerRequest => ApiKey.ProduceRequest.id
        case _: FetchRequest => ApiKey.FetchRequest.id
        case _: OffsetRequest => ApiKey.OffsetRequest.id
      }

    rq.writeTo(buffer)
    // Java 8/11 compatibility fix: ByteBuffer.rewind() returns Buffer in Java 8, ByteBuffer in Java 9+
    buffer.asInstanceOf[Buffer].rewind()
    (ByteVector.fromInt(sz+2) ++
      ByteVector.fromShort(apiId.toShort) ++
      ByteVector.view(buffer)).toBitVector
  }


  def serializeResponse(resp:RequestOrResponse): BitVector = {
    val sz = resp.sizeInBytes
    val buffer = ByteBuffer.allocate(sz)
    resp.writeTo(buffer)
    // Java 8/11 compatibility fix: ByteBuffer.rewind() returns Buffer in Java 8, ByteBuffer in Java 9+
    buffer.asInstanceOf[Buffer].rewind()
    (ByteVector.fromInt(sz) ++ ByteVector.view(buffer)).toBitVector
  }


}
