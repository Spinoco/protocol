package spinoco.protocol.kafka.codec

import java.util.Date

import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless.{::, HNil}
import shapeless.tag.@@
import spinoco.protocol.kafka.{PartitionId, ProtocolVersion, TopicName}
import spinoco.protocol.kafka.Request.OffsetsRequest
import spinoco.protocol.kafka.Response.{OffsetResponse, PartitionOffsetResponse}

/**
  * Created by pach on 30/05/17.
  */
object OffsetCodec {


  def requestCodec(version: Int): Codec[OffsetsRequest] = {
    (
      ("ReplicaId"          | kafkaBrokerId ) ::
      ("topics"             | kafkaArray(impl.topicRequestV9) ) // v10.1 seems to be not working yet, only V9 supported
    ).xmap(
      { case replicaId :: topics :: HNil => OffsetsRequest(replicaId, topics) }
      , { rq  => rq.replicaId :: rq.topics :: HNil }
    )
  }


  def responseCodec(protocol: ProtocolVersion.Value): Codec[OffsetResponse] = {
    // for now we do not support time-based offset lookup, only last/first offset query.
    val partitionCodec = impl.partitionResponseV9

    val topicEntry =
      ("Topic Name"           | kafkaTopicName) ~
      ("Partitions"           | kafkaArray(partitionCodec))

    kafkaArray(topicEntry)
    .as[OffsetResponse]
  }


  object impl {

    val topicRequestV9: Codec[(String @@ TopicName, Vector[(Int @@ PartitionId, Date, Option[Int])])] = {
      val partitionCodec: Codec[(Int @@ PartitionId, Date, Option[Int])] = {
          ("Partition Id"       | kafkaPartitionId)  ~
          ("Time"               | kafkaDate) ~
          ("MaxNumberOfOffsets" | int32)
        } exmap (
          { case  pid ~ time ~ no =>  Attempt.successful((pid, time, Some(no)))}
          , {
          case (pid, time, Some(no)) => Attempt.successful(((pid,  time), no))
          case (_, _, None) => Attempt.failure(Err("Missing MaxNumberOfOffsets, that must be defined in protocol prior V 0.10.1"))
        })

      ("Topic Name"           | kafkaTopicName) ~
      ("Partitions"           | kafkaArray(partitionCodec))
    }

    val topicRequestV101: Codec[(String @@ TopicName, Vector[(Int @@ PartitionId, Date, Option[Int])])] = {
      val partitionCodec: Codec[(Int @@ PartitionId, Date, Option[Int])] = {
        ("Partition Id"       | kafkaPartitionId)  ~
        ("Time"               | kafkaDate)
      } xmap (
        { case  pid ~ time  =>  (pid, time, None)}
        , { case (pid, time, _) => (pid, time) }
      )

      ("Topic Name"           | kafkaTopicName) ~
      ("Partitions"           | kafkaArray(partitionCodec))
    }


    val partitionResponseV9: Codec[PartitionOffsetResponse] = {
      ("Partition" | kafkaPartitionId) ::
      ("ErrorCode" | kafkaError) ::
      ("Offsets" | kafkaArray(kafkaOffset))
    } xmap (
      { case paritiionId :: error :: offsets :: HNil => PartitionOffsetResponse(paritiionId, error, new Date(0), offsets)}
      , { (resp : PartitionOffsetResponse)  => resp.partitionId :: resp.error :: resp.offsets :: HNil }
    )

    val partitionResponseV101: Codec[PartitionOffsetResponse] = {
      ("Partition" | kafkaPartitionId) ::
      ("ErrorCode" | kafkaError) ::
      ("TimeStamp" | kafkaDate) ::
      ("Offsets" | kafkaArray(kafkaOffset))
    } xmap (
      { case paritiionId :: error :: time :: offsets :: HNil => PartitionOffsetResponse(paritiionId, error, time, offsets) }
      , { (resp : PartitionOffsetResponse)  => resp.partitionId :: resp.error :: resp.timestamp :: resp.offsets :: HNil }
    )

  }

}
