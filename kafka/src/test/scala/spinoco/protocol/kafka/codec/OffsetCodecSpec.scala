package spinoco.protocol.kafka.codec

import java.util.Date

import org.apache.kafka.common.requests.{ListOffsetRequest, RequestHeader}
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import shapeless.tag
import spinoco.protocol.kafka.Response.PartitionOffsetResponse
import spinoco.protocol.kafka._

/**
  * Created by pach on 30/05/17.
  */
class OffsetCodecSpec extends CodecSpec {

  val kRequest = SerializationTestUtils.createTestOffsetFetchRequest
  val sRequest = RequestMessage(
    version = ProtocolVersion.Kafka_0_8
    , correlationId = 1
    , clientId = "client"
    , request = Request.OffsetsRequest(
      replicaId = tag[Broker](kRequest.replicaId)
      , topics = Vector(
        (tag[TopicName]("test1"), Vector(
          (tag[PartitionId](1), new Date(-1L), Some(1))
        ))
        )
      )
    )


  val kResponse = SerializationTestUtils.createTestOffsetFetchResponse
  val sResponse = ResponseMessage(
    correlationId = 1
    , Response.OffsetResponse(
      Vector((tag[TopicName]("test1"), Vector(
        PartitionOffsetResponse(tag[PartitionId](1), None, new Date(0), Vector(tag[Offset](0L), tag[Offset](10L), tag[Offset](20L)))
      )))
    )
  )

  "Offset API" - {
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
      MessageCodec.requestCodec.encode(sRequest).map{bv =>
        val bytes = bv.bytes.drop(4).toByteBuffer

        val _ = RequestHeader.parse(bytes)

        ListOffsetRequest.parse(bytes, 0).toString
      } shouldBe
      Attempt.successful { kRequest.toString }
    }

    "De-Serialize response" in {
      val serialized = serializeResponse(kResponse, 0.toShort)

      MessageCodec.responseCorrelationCodec.decode(serialized).flatMap { result =>
        MessageCodec.responseCodecFor(ProtocolVersion.Kafka_0_10_2, ApiKey.OffsetRequest).decode(result.value._2)
      } shouldBe Attempt.successful(DecodeResult(
        sResponse.response, BitVector.empty
      ))

    }

    "Serializes response" in {
      MessageCodec.responseCodecFor(ProtocolVersion.Kafka_0_8, ApiKey.OffsetRequest).encode(sResponse.response).flatMap {
        encoded => MessageCodec.responseCorrelationCodec.encode((ApiKey.OffsetRequest.id, encoded))
      }
      .mapErr(err => fail(s"Failed to encode OffsetResponse: $err"))
      .flatMap { encoded =>
        MessageCodec.responseCorrelationCodec.decode(encoded).flatMap { result =>
          MessageCodec.responseCodecFor(ProtocolVersion.Kafka_0_8, ApiKey.OffsetRequest).decode(result.value._2)
        }
      } shouldBe Attempt.successful(DecodeResult(
        sResponse.response, BitVector.empty
      ))

    }

  }

}
