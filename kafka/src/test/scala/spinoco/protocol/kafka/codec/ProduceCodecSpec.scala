package spinoco.protocol.kafka.codec

import java.util.concurrent.TimeUnit

import org.apache.kafka.common.requests.RequestHeader
import shapeless.tag.@@
import shapeless.{the => The}
import spinoco.protocol.kafka.Request.{ProduceRequest, RequiredAcks}
import spinoco.protocol.kafka._

import collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration


class ProduceCodecSpec extends CodecSpec {

  "Produce API" - {

    // TODO fix this test, currently when we serialize the req from kafka, we get different byte vector
    // than when we serialize it manually, however is being read by kafka alright.
//    "De-Serialize request" in {
//      val req = SerializationTestUtils.createTestProducerRequest
//      val result =  MessageCodec.requestCodec.decode(serializeRequest(req))
//
//      result.map{r => r.copy(value = r.value.copy(request = align(r.value.request))) }  shouldBe
//        Attempt.successful(DecodeResult(
//          kafka2spinoco(req)
//          , BitVector.empty
//        ))
//    }


    "Serializes the request in " in {
      val req = SerializationTestUtils.createTestProducerRequest
      val rq = kafka2spinoco(req)
      val result = MessageCodec.requestCodec.encode(rq)
      val serialized = result.getOrElse(fail(s"Failed to encode the request $rq"))

      val bytes = serialized.bytes.drop(4).toByteBuffer

      val _ = RequestHeader.parse(bytes)

      kafka2spinoco(org.apache.kafka.common.requests.ProduceRequest.parse(bytes, 0)) shouldBe rq

    }
  }



  def kafka2spinoco(in: org.apache.kafka.common.requests.ProduceRequest): RequestMessage = {
    RequestMessage(
      version =  ProtocolVersion.Kafka_0_8
      , correlationId = 1
      , clientId = "client"
      , request = ProduceRequest(
        requiredAcks = RequiredAcks(in.acks().toInt)
        , timeout = FiniteDuration(in.timeout(), TimeUnit.MILLISECONDS)
        , messages = SerializationTestUtils.kafka2SpinocoData(in.partitionRecordsOrFail().asScala)
      )
    )
  }

  implicit val ordPartId: Ordering[Int @@ PartitionId] = The[Ordering[Int]].on(i => i:Int)
  implicit val ordTopicName: Ordering[String @@ TopicName] = The[Ordering[String]].on(s => s:String)

  def align(request: Request):Request = request match {
    case produce: ProduceRequest =>
      produce.copy(
        messages = produce.messages.sortBy(_._1).map{ case (t,m) => t -> m.sortBy(_._1)}
      )
    case other => other
  }


}
