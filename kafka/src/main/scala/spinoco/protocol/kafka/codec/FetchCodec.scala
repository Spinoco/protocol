package spinoco.protocol.kafka.codec

import scodec.Codec
import scodec.codecs._
import shapeless.{::, HNil}
import shapeless.tag.@@
import spinoco.protocol.kafka.{Offset, PartitionId, ProtocolVersion, TopicName}
import spinoco.protocol.kafka.Request.FetchRequest
import spinoco.protocol.kafka.Response.{FetchResponse, PartitionFetchResult}
import spinoco.protocol.common.codec._

import scala.concurrent.duration._

/**
  * Created by pach on 27/08/16.
  */
object FetchCodec {


  val requestCodec:Codec[FetchRequest] = {
    (
      ("ReplicaId"          | kafkaBrokerId ) ::
      ("MaxWaitTime"        | durationIntMs(int32)) ::
      ("MinBytes"           | int32) ::
      ("Topics"             | impl.topics)
    ).xmap(
      { case replicaId :: maxWaitTime :: minBytes :: topics :: HNil =>
        FetchRequest(replicaId, maxWaitTime, minBytes, topics)
      }
      , rq => rq.replica :: rq.maxWaitTime :: rq.minBytes :: rq.topics :: HNil
    )
  }


  def responseCodec(version:ProtocolVersion.Value):Codec[FetchResponse] = {
    version match {
      case ProtocolVersion.Kafka_0_8 =>
        "FetchResponse V0" | impl.fetchResponse.xmap(FetchResponse(_,None), _.data)

      case ProtocolVersion.Kafka_0_10 |
           ProtocolVersion.Kafka_0_10_1 |
           ProtocolVersion.Kafka_0_9 =>
        (
          ("ThrottleTime"     | durationIntMs(int32) ) ~
          ("data"             | impl.fetchResponse)
        ).xmap(
          { case (time, resp) => FetchResponse(resp,Some(time))}
          , resp => resp.throttleTime.getOrElse(0.millis) -> resp.data
        )

    }
  }


  object impl {

    val requestPartitionFetch : Codec[(Int @@ PartitionId, Long @@ Offset, Int)] = {
      import shapeless.syntax.std.tuple._
      (
        ("Partition"    | kafkaPartitionId) ::
        ("FetchOffset"  | kafkaOffset) ::
        ("MaxBytes"     | int32)
      ).xmap(_.tupled, _.productElements)
    }

    val topics: Codec[Vector[(String @@ TopicName, Vector[(Int @@ PartitionId, Long @@ Offset, Int)])]] = {
      kafkaArray(
        ("TopicName"    | kafkaTopicName ) ~
        ("Partitions"   | kafkaArray(impl.requestPartitionFetch))
      )

    }



    val partitionFetchResult: Codec[Vector[PartitionFetchResult]] = {
      kafkaArray(
        (
          ("Partition"            | kafkaPartitionId) ::
          ("ErrorCode"            | kafkaError) ::
          ("HighwaterMarkOffset"  | kafkaOffset) ::
          ("MessageSetSize"       | variableSizeBytes(int32 , MessageSetCodec.messageSetCodec))
        ).xmap(
          { case partitionId :: error :: offset :: messageSet :: HNil =>
            PartitionFetchResult(partitionId,error,offset,messageSet)
          }
          , r => r.partitionId :: r.error :: r.highWMOffset :: r.messages :: HNil
        )
      )
    }

    val fetchResponse : Codec[Vector[(String @@ TopicName, Vector[PartitionFetchResult])]] = {
      kafkaArray(
        ("TopicName"      | kafkaTopicName ) ~
        ("Partitions"     | partitionFetchResult)
      )
    }

  }


}
