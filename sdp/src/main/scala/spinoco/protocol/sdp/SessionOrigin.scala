package spinoco.protocol.sdp

/**
  * Created by pach on 04/03/17.
  */
case class SessionOrigin(
  userName: String
  , sessionId: String
  , sessionVersion: String
  , netType: NetType.Value
  , addressType: AddressType.Value
  , unicastAddress: String
)

object NetType extends  Enumeration {
  val IN = Value
}


object AddressType extends Enumeration {
  val IP4 = Value
  val IP6 = Value
}
