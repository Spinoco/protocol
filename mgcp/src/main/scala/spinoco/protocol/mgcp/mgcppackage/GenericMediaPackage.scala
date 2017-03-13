package spinoco.protocol.mgcp.mgcppackage

import shapeless.tag
import shapeless.tag.@@
import spinoco.protocol.mgcp.{PackageEvent, PackageName}


object GenericMediaPackage {

  val id: String @@ PackageName = tag[PackageName]("g")

}


case class GenericPackageEvent(name: String, params:List[String]) extends PackageEvent


object GenericPackageEvent {

  val ConfirmTone = GenericPackageEvent("cf", Nil)
  val CongestionTone = GenericPackageEvent("cg", Nil)
  val FaxTone = GenericPackageEvent("ft", Nil)
  val InterceptTone = GenericPackageEvent("it", Nil)
  val LongDurationConnection = GenericPackageEvent("ld", Nil)
  val ModemTone = GenericPackageEvent("mt", Nil)
  val OperationComplete = GenericPackageEvent("oc", Nil)
  val OperationFailure = GenericPackageEvent("of", Nil)
  def PatternDetected(pattern: String) = GenericPackageEvent("pat", List(pattern))
  val PreemptionTone = GenericPackageEvent("pt", Nil)
  def RingBack(rb: String) = GenericPackageEvent("rbk", List(rb))
  val RingBackTone = GenericPackageEvent("rt", Nil)
}
