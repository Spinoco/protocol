package spinoco.protocol.mgcp

/**
  * Created by pach on 09/03/17.
  */
object MGCPParameterInfoName extends Enumeration {

  val BearerInformation             = Value("B")
  val CallId                        = Value("C")
  val Capabilities                  = Value("A")
  val ConnectionId                  = Value("I")
  val ConnectionMode                = Value("M")
  val ConnectionParameters          = Value("P")
  val DetectEvents                  = Value("T")
  val DigitMap                      = Value("D")
  val EventStates                   = Value("ES")
  val LocalConnectionOptions        = Value("L")
  val MaxMGCPDatagram               = Value("MD")
  val NotifiedEntity                = Value("N")
  val ObservedEvents                = Value("O")
  val PackageList                   = Value("PL")
  val QuarantineHandling            = Value("Q")
  val ReasonCode                    = Value("E")
  val RequestedEvents               = Value("R")
  val RequestedInfo                 = Value("F")
  val RequestIdentifier             = Value("X")
  val ResponseAck                   = Value("K")
  val RestartDelay                  = Value("RD")
  val RestartMethod                 = Value("RM")
  val SignalRequests                = Value("S")
  val SpecificEndPointId            = Value("Z")
  val RemoteConnectionDescriptor    = Value("RC")
  val LocalConnectionDescriptor     = Value("LC")
}
