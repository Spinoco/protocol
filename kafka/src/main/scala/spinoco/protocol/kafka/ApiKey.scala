package spinoco.protocol.kafka

object ApiKey extends Enumeration {
  val ProduceRequest =          Value(0)
  val FetchRequest =            Value(1)
  val MetadataRequest =         Value(3)
}
