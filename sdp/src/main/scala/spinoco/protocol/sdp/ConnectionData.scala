package spinoco.protocol.sdp


case class ConnectionData(
  netType : NetType.Value
  , addressType: AddressType.Value
  , address: String
  , ttl: Option[Int]
  , count: Option[Int]
)


