package spinoco.protocol.mgcp


object ConnectionParameter {
  case class PacketSent(value: Int) extends ConnectionParameter
  case class OctetsSent(value: Int) extends ConnectionParameter
  case class PacketReceived(value: Int) extends ConnectionParameter
  case class OctetsReceived(value: Int) extends ConnectionParameter
  case class PacketLost(value: Int) extends ConnectionParameter
  case class Jitter(value: Int) extends ConnectionParameter
  case class AvgLatency(value: Int) extends ConnectionParameter
}


sealed trait ConnectionParameter
