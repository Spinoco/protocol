package spinoco.protocol.mgcp

object ConnectionModeType {
  object SendOnly extends ConnectionModeType
  object ReceiveOnly extends ConnectionModeType
  object SendAndReceive extends ConnectionModeType
  object Inactive extends ConnectionModeType
  object Conference extends ConnectionModeType
  object Loopback extends ConnectionModeType
  object ConnectionTest extends ConnectionModeType
  object NetworkLoop extends ConnectionModeType
  object NetworkTest extends ConnectionModeType
  case class Extension(pkg: String, mode:String) extends ConnectionModeType
}


sealed trait ConnectionModeType
