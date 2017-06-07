package spinoco.protocol.kafka

import java.util.Date

import shapeless.tag.@@

import scala.concurrent.duration.FiniteDuration

/**
  * Response for Kafka Broker
  */
trait Response


object Response {

  /**
    * Response to [[spinoco.protocol.kafka.Request.MetadataRequest]]
    * @param brokers  All brokers known
    * @param topics   All topics known
    */
  case class MetadataResponse(
    brokers: Vector[Broker]
    , topics: Vector[TopicMetadata]
  ) extends Response

  /**
    * Response to [[spinoco.protocol.kafka.Request.ProduceRequest]]
    * Contains map per topic and partition.
    *
    * @param data           Contains result of each produce response. Not guaranteed to be in same order as request.
    * @param throttleTime   If the request was throttled, this contains time how long it was throttled (since kafka 0.9.0)
    */
  case class ProduceResponse(
    data: Vector[(String @@ TopicName, Vector[(Int @@ PartitionId, PartitionProduceResult)])]
    , throttleTime:Option[FiniteDuration]
  ) extends Response

  /**
    * Contains result of the produce for single partition
    *
    * @param error      If nonempty, produce failed
    * @param offset     Contains offset of first published message
    * @param time       If LogAppendTime is used for the topic, this is the timestamp assigned by the broker to the message set.
    *                   All the messages in the message set have the same timestamp. If CreateTime is used, this field is always None.
    *                   The producer can assume the timestamp of the messages in the produce request has been accepted by the broker if there is no error code returned.
    *                   Unit is milliseconds since beginning of the epoch (midnight Jan 1, 1970 (UTC)).
    *                   Available since kafka 0.10.0
    */
  case class PartitionProduceResult(
    error: Option[ErrorType.Value]
    , offset:Long @@ Offset
    , time:Option[Date]
  )


  /**
    * Response to [[spinoco.protocol.kafka.Request.FetchRequest]]
    *
    *
    * @param data           Contains data of messages fetched
    * @param throttleTime   If the request was throttled, this contains time how long it was throttled (since kafka 0.9.0)
    */
  case class FetchResponse(
    data:Vector[(String @@ TopicName, Vector[PartitionFetchResult])]
    , throttleTime:Option[FiniteDuration]
  ) extends Response

  /**
    * Contains fetch result for given partition
    *
    * @param partitionId    Id of partition
    * @param error          If nonempty, fetch resulted in error
    * @param highWMOffset   The offset at the end of the log for this partition. This can be used by the client to determine how many messages behind the end of the log they are.
    * @param messages       Messages fetched.
    */
  case class PartitionFetchResult(
   partitionId: Int @@ PartitionId
   , error: Option[ErrorType.Value]
   , highWMOffset: Long @@ Offset
   , messages:Vector[Message]
 )


  /**
    * Response to the offset query. Response for topic queries by client
    * @param data Data containing reposne to offset query
    */
  case class OffsetResponse(
    data: Vector[(String @@ TopicName, Vector[PartitionOffsetResponse])]
  ) extends Response


  /**
    * Reposne data for Offset query for an partition
    * @param partitionId  Id of partition
    * @param error        If nonempty, reposne failed
    * @param timestamp    If query contained a timestamp, this will indicate offset for given timestamp. 0 in case of protocolV9
    * @param offsets      Offsets of chunks for given partition
    */
  case class PartitionOffsetResponse(
    partitionId: Int @@ PartitionId
    , error: Option[ErrorType.Value]
    , timestamp: Date
    , offsets: Vector[Long @@ Offset]
  )


}
