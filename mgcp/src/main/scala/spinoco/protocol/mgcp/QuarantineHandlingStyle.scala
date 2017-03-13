package spinoco.protocol.mgcp

/**
  * Created by pach on 09/03/17.
  */
object QuarantineHandlingStyle extends Enumeration {

  val LoopControlStep = Value("step")
  val LoopControlLoop = Value("loop")
  val ProcessControlProcess = Value("process")
  val ProcessControlDiscard = Value("discard")
}
