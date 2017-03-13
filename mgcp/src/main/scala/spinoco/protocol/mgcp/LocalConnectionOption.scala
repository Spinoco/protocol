package spinoco.protocol.mgcp

object LocalConnectionOption {

  case class PacketizationPeriod(period:Int) extends LocalConnectionOption
  case class CompressionAlgorithm(algorithm: String) extends LocalConnectionOption
  case class Bandwidth(from: Int, to: Option[Int]) extends LocalConnectionOption
  case class EchoCancel(enable: Boolean) extends LocalConnectionOption
  case class GainControl(gain: Option[Int]) extends LocalConnectionOption
  case class SilenceSuppression(enable: Boolean) extends LocalConnectionOption
  case class TypeOfService(tos: String) extends LocalConnectionOption
  case class ResourceReservation(value: String) extends LocalConnectionOption
  case class TypeOfNetwork(nt: NetworkType.Value, supported: List[NetworkType.Value]) extends LocalConnectionOption
  case class VendorOption(tag:String, value: String) extends LocalConnectionOption

}


sealed trait LocalConnectionOption extends CapabilityValue



object NetworkType extends Enumeration {
  val IN, ATM, LOCAL = Value
}


object CapabilityValue {

  case class SupportedPackages(packages: List[String]) extends CapabilityValue
  case class SupportedModes(modes: List[ConnectionModeType]) extends CapabilityValue

}


sealed trait CapabilityValue