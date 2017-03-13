package spinoco.protocol.sdp

object SDPParameter {

  case class Version(v: Int) extends SDPParameter


}


sealed trait SDPParameter
