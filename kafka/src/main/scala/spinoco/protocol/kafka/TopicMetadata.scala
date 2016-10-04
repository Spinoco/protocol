package spinoco.protocol.kafka

import shapeless.tag.@@

/**
  * Metadata for supplied topic
 *
  * @param error        If present there was a failure to query metadata, or topic is in erroneous state
  * @param name         Name of the topic
  * @param partitions   metadata of the partitions
  */
case class TopicMetadata(
  error: Option[ErrorType.Value]
  , name: String @@ TopicName
  , partitions: Vector[PartitionMetadata]
)


/**
  * Single partition metadata
  * @param error        If present, metadata query failed for this partition, or partition is in erroneous state
  * @param id           Id of the partition
  * @param leader       Elected leader
  * @param replicas     All available replicas
  * @param isr          All in sync replicas
  */
case class PartitionMetadata(
  error: Option[ErrorType.Value]
  , id: Int @@ PartitionId
  , leader: Option[Int @@ Broker]
  , replicas: Vector[Int @@ Broker]
  , isr: Vector[Int @@ Broker]
)