package spinoco.protocol.mgcp.mgcppackage
import shapeless.tag
import shapeless.tag.@@
import spinoco.protocol.mgcp.{PackageEvent, PackageName}


object LinePackage {

  val id: String @@ PackageName = tag[PackageName]("l")

}

case class LinePackageEvent(name: String, params:List[String]) extends PackageEvent

object LinePackageEvent  {

  val AnswerTone          = LinePackageEvent("aw", Nil)
  val BusyTone            = LinePackageEvent("bz", Nil)
  def CallerId(time:String, number: String, name: String):LinePackageEvent = LinePackageEvent("ci", List(time,number,name))
  val DialTone            = LinePackageEvent("dl", Nil)
  val ErrorTone           = LinePackageEvent("e", Nil)
  val OffHookTransition   = LinePackageEvent("hd", Nil)
  val FlashHook           = LinePackageEvent("hf", Nil)
  val OnHoldTone          = LinePackageEvent("ht", Nil)
  val OnHookTransition    = LinePackageEvent("hu", Nil)
  val LineSideAnswer      = LinePackageEvent("lsa", Nil)
  val MessageWaiting      = LinePackageEvent("mwi", Nil)
  val NetworkBusy         = LinePackageEvent("nbz", Nil)
  val OperationComplete   = LinePackageEvent("oc", Nil)
  val OperationFailure    = LinePackageEvent("of", Nil)
  val NetworkDisconnect   = LinePackageEvent("osi", Nil)
  val OffHookWarningTone  = LinePackageEvent("ot", Nil)
  val PromptTone          = LinePackageEvent("p", Nil)
  val Ringing             = LinePackageEvent("rg", Nil)
  def DistinctiveRinging(no: Int)= LinePackageEvent(s"r$no", Nil)
  val ReorderTone             = LinePackageEvent("ro", Nil)
  val Ringsplash              = LinePackageEvent("rs", Nil)
  def DistinctiveTonePattern(pattern:String)  = LinePackageEvent("s", List(pattern))
  def SpecialInformationTone(tone:String)  = LinePackageEvent("sit", List(tone))
  val StutterDialTone         = LinePackageEvent("sl", Nil)
  val AlertingTone            = LinePackageEvent("v", Nil)
  val VisualMessageWaiting    = LinePackageEvent("vmwi", Nil)
  val CallWaitingTone         = LinePackageEvent("wt", Nil)
  def AlternativeCallWT(no: Int) = LinePackageEvent(s"wt$no", Nil)
  val RecorderWarningTone     = LinePackageEvent("y", Nil)
  val CallingCardServiceTone  = LinePackageEvent("z", Nil)


}
