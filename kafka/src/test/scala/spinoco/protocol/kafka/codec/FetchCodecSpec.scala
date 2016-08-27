package spinoco.protocol.kafka.codec

import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult}
import shapeless.tag
import spinoco.protocol.kafka.Request.FetchRequest
import spinoco.protocol.kafka._

import concurrent.duration._

class FetchCodecSpec extends CodecSpec {

  val kRequest = SerializationTestUtils.createTestFetchRequest
  val sRequest = RequestMessage(
    version = ProtocolVersion.Kafka_0_10
    , correlationId = kRequest.correlationId
    , clientId = Option(kRequest.clientId)
    , request = FetchRequest(
      replica = tag[BrokerId](kRequest.replicaId)
      , maxWaitTime = kRequest.maxWait.millis
      , minBytes = kRequest.minBytes
      , topics = Vector(
        (tag[TopicName]("test2"), Vector(
          (tag[PartitionId](3),tag[Offset](4000),100)
          , (tag[PartitionId](0),tag[Offset](1000),100)
          , (tag[PartitionId](2),tag[Offset](3000),100)
          , (tag[PartitionId](1),tag[Offset](2000),100)))
        , (tag[TopicName]("test1"), Vector(
          (tag[PartitionId](3),tag[Offset](4000),100)
          , (tag[PartitionId](1),tag[Offset](2000),100)
          , (tag[PartitionId](2),tag[Offset](3000),100)
          , (tag[PartitionId](0),tag[Offset](1000),100)))
      )
    )
  )


  "Fetch API" - {
    "De-Serialize request" in {

      val serialized1 = serializeRequest(kRequest)
      val result = MessageCodec.requestCodec.decode(serialized1)

      result shouldBe Attempt.successful(
        DecodeResult(
          sRequest
          , BitVector.empty
        )
      )
    }

    "Serializes request" in {
      MessageCodec.requestCodec.encode(sRequest).map(bv => kafka.api.FetchRequest.readFrom(bv.bytes.drop(4+2).toByteBuffer)) shouldBe
      Attempt.successful { kRequest }
    }

  }

}
