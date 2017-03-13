package spinoco.protocol.mgcp


object RestartMethodType extends Enumeration {
  val Graceful = Value("graceful")
  val Forced = Value("forced")
  val Restart = Value("restart")
  val Disconnected = Value("disconnected")
  val CancelGraceful = Value("cancel-graceful")
}
