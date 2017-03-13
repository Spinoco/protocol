package spinoco.protocol.sdp

import scala.concurrent.duration.FiniteDuration

/**
  * Various Supported SDP Attributes
  */
object Attribute {

  case class Category(name: String) extends Attribute
  case class Keywords(keywords: String) extends Attribute
  case class Tool(name: String) extends Attribute
  case class PacketTime(dur: FiniteDuration) extends Attribute
  case class MaxPacketTime(dur: FiniteDuration) extends Attribute
  case class RtpMap(tpe: Int, name: String, clock: Int, channels:Option[Int]) extends Attribute
  case class MediaDirection(tpe: MediaDirectionType.Value) extends Attribute
  case class Orientation(orientation: OrientationType.Value) extends Attribute
  case class ConferenceType(tpe: String) extends Attribute
  case class CharacterSet(set: String) extends Attribute
  case class Language(language: String) extends Attribute
  case class SDPLanguage(language: String) extends Attribute
  case class FrameRate(frames:Int, fraction: Option[Int]) extends Attribute
  case class Quality(quality: Int) extends Attribute
  case class FormatParams(format: String, params: String) extends Attribute
}



sealed trait Attribute


object MediaDirectionType extends Enumeration {

  val ReceiveOnly = Value("recvonly")
  val SendOnly = Value("sendonly")
  val SendAndReceive = Value("sendrecv")
  val Inactive = Value("inactive")

}


object OrientationType extends Enumeration {
  val Portrait = Value("portrait")
  val Landscape = Value("landscape")
  val Seascape = Value("seascape")
}