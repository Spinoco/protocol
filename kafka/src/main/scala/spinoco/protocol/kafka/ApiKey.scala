package spinoco.protocol.kafka

object ApiKey extends Enumeration {
  val ProduceRequest =          Value(0)
  val FetchRequest =            Value(1)
  val MetadataRequest =         Value(3)

  def forRequest(request:Request):ApiKey.Value = request match {
    case _:Request.ProduceRequest => ProduceRequest
    case _:Request.FetchRequest => FetchRequest
    case _:Request.MetadataRequest => MetadataRequest
  }
}
