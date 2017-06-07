package spinoco.protocol.kafka.codec

import java.util.Date

import scodec.Codec
import scodec.codecs._
import shapeless.{::, HNil}
import shapeless.tag._
import spinoco.protocol.kafka.Request.{ProduceRequest, RequiredAcks}
import spinoco.protocol.kafka._
import spinoco.protocol.common.codec._
import spinoco.protocol.kafka.Response.{PartitionProduceResult, ProduceResponse}

import scala.concurrent.duration._


object ProduceCodec {


  val requestCodec : Codec[ProduceRequest] = {
    "Produce Request" | (
      ("Required Acks"      | enumerated(int16,RequiredAcks)) ::
      ("Timeout"            | durationIntMs(int32)) ::
      ("Topic Messages"     | impl.topicMessagesCodec )
    ).xmap(
      { case acks :: timeout :: messages :: HNil => ProduceRequest(acks,timeout, messages) }
      , rq => rq.requiredAcks :: rq.timeout :: rq.messages :: HNil
    )
  }

  def produceResponseCodec(version:ProtocolVersion.Value) : Codec[ProduceResponse] = {
    version match {
      case ProtocolVersion.Kafka_0_8 => "Produce Response V0" | impl.produceResponseV0
      case ProtocolVersion.Kafka_0_9 => "Produce Response V1" | impl.produceResponseV1
      case ProtocolVersion.Kafka_0_10 |
           ProtocolVersion.Kafka_0_10_1 => "Produce Response V2" | impl.produceResponseV2
    }
  }


  object impl {

    val partitionMessagesCodec: Codec[Vector[(Int @@ PartitionId,Vector[Message])]] = {
      kafkaArray(
        ("Partition Id"     | kafkaPartitionId)  ~
        ( "Messages"        | variableSizeBytes("Message Set Size" | int32, MessageSetCodec.messageSetCodec))
      )
    }

    val topicMessagesCodec : Codec[Vector[(String @@ TopicName,Vector[(Int @@ PartitionId,Vector[Message])])]] = {
      kafkaArray(
        ("Topic Name"           | kafkaTopicName) ~
        ("Partition Messages"   | partitionMessagesCodec)
      )
    }

    val topicResponseV0V1:Codec[Vector[(Int @@ PartitionId, PartitionProduceResult)]] = {
      kafkaArray(
        ( ("Partition Id"       | kafkaPartitionId) ::
          ("Error Code"         | kafkaError) ::
          ("Offset"             | kafkaOffset)
        )
        .xmap(
          { case pid :: err :: offset :: HNil => (pid,PartitionProduceResult(err,offset,None)) }
          , {case (pid,rslt) => pid :: rslt.error :: rslt.offset :: HNil}
        )
      )
    }

    val topicResponseV2:Codec[Vector[(Int @@ PartitionId, PartitionProduceResult)]] = {
      kafkaArray(
        ( ("Partition Id"       | kafkaPartitionId) ::
          ("Error Code"         | kafkaError) ::
          ("Offset"             | kafkaOffset) ::
          ("TimeStamp"          | epochTimestamp)
        )
        .xmap(
          { case pid :: err :: offset :: ts :: HNil => (pid,PartitionProduceResult(err,offset,Some(ts).filter(_.getTime >= 0))) }
          , {case (pid,rslt) => pid :: rslt.error :: rslt.offset :: rslt.time.getOrElse(new Date(-1)) :: HNil}
        )
      )
    }

    val produceResponseV0:Codec[ProduceResponse] = {
      "Messages" | kafkaArray(
        ("Topic Name"           | kafkaTopicName) ~
        ("Topic Response"       | topicResponseV0V1)
      ).xmap(
        messages => ProduceResponse(messages,None)
        , rsp => rsp.data
      )
    }

    val produceResponseV1:Codec[ProduceResponse] = {
      ( "Messages" | kafkaArray(
          ("Topic Name"           | kafkaTopicName) ~
          ("Topic Response"       | topicResponseV0V1)
        ) ~
        ("ThrottleTime" | durationIntMs(int32))
      ).xmap(
        {case (messages, throttle) => ProduceResponse(messages,Some(throttle)) }
        , rsp => (rsp.data, rsp.throttleTime.getOrElse(0.millis))
      )
    }

    val produceResponseV2:Codec[ProduceResponse] = {
        ( "Messages" | kafkaArray(
          ("Topic Name"             | kafkaTopicName) ~
          ("Topic Response"         | topicResponseV2)
        ) ~
        ("ThrottleTime" | durationIntMs(int32))
      ).xmap(
        {case (messages, throttle) => ProduceResponse(messages,Some(throttle)) }
        , rsp => (rsp.data, rsp.throttleTime.getOrElse(0.millis))
      )
    }





  }


}
