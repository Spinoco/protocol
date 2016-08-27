package spinoco.protocol.kafka

/**
  * Versions of kafka for protocol compatibility
  */
object ProtocolVersion extends Enumeration{
  val Kafka_0_8 = Value
  val Kafka_0_9 = Value
  val Kafka_0_10 = Value
}
