package spinoco.protocol.mgcp.mgcppackage

import shapeless.tag
import shapeless.tag.@@
import spinoco.protocol.mgcp.{PackageEvent, PackageName}

/**
  * Created by pach on 12/03/17.
  */
object DTMFPackage {

  val id: String @@ PackageName = tag[PackageName]("d")

}

sealed trait DTMFPackageEvent extends PackageEvent

case class DTMFEvent(name: String, params:List[String]) extends DTMFPackageEvent
case class DTMFPackagePatternEvent(pattern:String) extends DTMFPackageEvent


object DTMFPackageEvent {

  val DTMF0 = DTMFEvent("0", Nil)
  val DTMF1 = DTMFEvent("1", Nil)
  val DTMF2 = DTMFEvent("2", Nil)
  val DTMF3 = DTMFEvent("3", Nil)
  val DTMF4 = DTMFEvent("4", Nil)
  val DTMF5 = DTMFEvent("5", Nil)
  val DTMF6 = DTMFEvent("6", Nil)
  val DTMF7 = DTMFEvent("7", Nil)
  val DTMF8 = DTMFEvent("8", Nil)
  val DTMF9 = DTMFEvent("9", Nil)
  val `DTMF#` = DTMFEvent("#", Nil)
  val `DTMF*` = DTMFEvent("*", Nil)
  val DTMFA = DTMFEvent("A", Nil)
  val DTMFB = DTMFEvent("B", Nil)
  val DTMFC = DTMFEvent("C", Nil)
  val DTMFD = DTMFEvent("D", Nil)
  def ToneDuration(params:String) = DTMFEvent("DD", List(params))
  def OOSignal(params:String) = DTMFEvent("DO", List(params))
  val LongDuration = DTMFEvent("L", Nil)
  val OperationComplete = DTMFEvent("oc", Nil)
  val OperationFailure = DTMFEvent("of", Nil)
  val InterDigitTimer = DTMFEvent("T", Nil)

}


