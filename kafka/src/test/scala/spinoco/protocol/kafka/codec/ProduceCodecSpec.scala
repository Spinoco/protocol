package spinoco.protocol.kafka.codec

import java.util.concurrent.TimeUnit

import kafka.api.ProducerRequest
import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult}
import shapeless.tag.@@
import shapeless.{the => The}
import spinoco.protocol.kafka.Request.ProduceRequest
import spinoco.protocol.kafka._

import scala.concurrent.duration.FiniteDuration


class ProduceCodecSpec extends CodecSpec {

  "Produce API" - {

    "De-Serialize request" in {
      val req = SerializationTestUtils.createTestProducerRequest
      val result =  MessageCodec.requestCodec.decode(serializeRequest(req))

      result.map{r => r.copy(value = r.value.copy(request = align(r.value.request))) }  shouldBe
        Attempt.successful(DecodeResult(
          kafka2spinoco(req)
          , BitVector.empty
        ))

    }


    "Serializes the request in " in {
      val req = SerializationTestUtils.createTestProducerRequest
      val rq = kafka2spinoco(req)
      val result = MessageCodec.requestCodec.encode(rq)
      val serialized = result.getOrElse(fail(s"Failed to encode the request $rq"))

      kafka2spinoco(ProducerRequest.readFrom(serialized.bytes.drop(4+2).toByteBuffer)) shouldBe rq

    }



  }



  def kafka2spinoco(in:ProducerRequest):RequestMessage = RequestMessage(
    version =  ProtocolVersion(in.versionId)
    , correlationId = in.correlationId
    , clientId = Some(in.clientId)
    , request = ProduceRequest(
      requiredAcks = in.requiredAcks
      , timeout = FiniteDuration(in.ackTimeoutMs, TimeUnit.MILLISECONDS)
      , messages = SerializationTestUtils.kafka2SpinocoData(in.data)
    )
  )

  implicit val ordPartId: Ordering[Int @@ PartitionId] = The[Ordering[Int]].on(i => i:Int)
  implicit val ordTopicName: Ordering[String @@ TopicName] = The[Ordering[String]].on(s => s:String)

  def align(request:Request):Request = request match {
    case produce: ProduceRequest =>
      produce.copy(
        messages = produce.messages.sortBy(_._1).map{ case (t,m) => t -> m.sortBy(_._1)}
      )
    case other => other
  }


}
