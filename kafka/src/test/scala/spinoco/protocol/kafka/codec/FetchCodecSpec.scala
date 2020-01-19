package spinoco.protocol.kafka.codec

import org.apache.kafka.common.requests.RequestHeader
import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, DecodeResult}
import shapeless.tag
import spinoco.protocol.kafka.Request.FetchRequest
import spinoco.protocol.kafka._

import scala.concurrent.duration._

class FetchCodecSpec extends CodecSpec {

  val kRequest = SerializationTestUtils.createTestFetchRequest
  val sRequest = RequestMessage(
    version = ProtocolVersion.Kafka_0_8
    , correlationId = 1
    , clientId = "client"
    , request = FetchRequest(
      replica = tag[Broker](kRequest.replicaId)
      , maxWaitTime = kRequest.maxWait.millis
      , minBytes = kRequest.minBytes
      , maxBytes = None
      , topics = Vector(
        (tag[TopicName]("test1"), Vector(
          (tag[PartitionId](0),tag[Offset](1000),100)
          , (tag[PartitionId](1),tag[Offset](2000),100)
          , (tag[PartitionId](2),tag[Offset](3000),100)
          , (tag[PartitionId](3),tag[Offset](4000),100)
        ))
        , (tag[TopicName]("test2"), Vector(
          (tag[PartitionId](0),tag[Offset](1000),100)
          , (tag[PartitionId](1),tag[Offset](2000),100)
          , (tag[PartitionId](2),tag[Offset](3000),100)
          , (tag[PartitionId](3),tag[Offset](4000),100)
        ))
      )
    )
  )


  "Fetch API" - {
    "De-Serialize request - fetch" in {

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
      MessageCodec.requestCodec.encode(sRequest).map { bv =>
        val bytes = bv.bytes.drop(4).toByteBuffer

        val _ = RequestHeader.parse(bytes)

        // We are in java world, objects do not equal
        org.apache.kafka.common.requests.FetchRequest.parse(bytes, 0).toString
      } shouldBe Attempt.successful { kRequest.toString }
    }

    "De-Serialize large request with incomplete offset" in {
      /** largeIncompleteResponse contains 22026 messages and 1 incomplete**/
      val stream: java.net.URL = getClass.getResource("/largeIncompleteOffset")
      val lines = scala.io.Source.fromURL(stream).getLines().toSeq
      lines.headOption.map { line =>
        val bv: ByteVector = ByteVector.fromValidHex(line)
        val codec = FetchCodec.responseCodec(ProtocolVersion.Kafka_0_8)
        val result = codec.decode(bv.drop(4).bits).map(_.value.data.flatMap(_._2.map(_.messages.size)))
        result shouldBe Attempt.successful(Vector(21519))
      } getOrElse {
        fail("Failed to read largeIncompleteOffset.")
      }
    }

    "De-Serialize large request with incomplete message" in {
      /** largeIncompleteResponse contains 22026 messages and 1 incomplete**/
      val stream: java.net.URL = getClass.getResource("/largeIncompleteResponse")
      val lines = scala.io.Source.fromURL(stream).getLines().toSeq
      lines.headOption.map { line =>
        val bv: ByteVector = ByteVector.fromValidHex(line)
        val codec = FetchCodec.responseCodec(ProtocolVersion.Kafka_0_8)
        val result = codec.decode(bv.drop(4).bits).map(_.value.data.flatMap(_._2.map(_.messages.size)))
        result shouldBe Attempt.successful(Vector(22026))
      } getOrElse {
        fail("Failed to read largeIncompleteResponse.")
      }
    }

  }

}
