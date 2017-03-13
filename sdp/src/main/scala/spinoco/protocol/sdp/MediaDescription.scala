package spinoco.protocol.sdp

/**
  * Description of the Media transferred
  * @param tpe                Type of the media
  * @param port               Port where the media are available
  * @param portCount          Count of the ports
  * @param protocol           Media transport protocol
  * @param format             Media format ordered by their preference
  * @param information        Information
  * @param connectionData     Connection data ofr the media, if differ from one supplied at session level
  * @param bandwidth          Bandwidth information for this media
  * @param attributes         Attributes for this media.
  */
case class MediaDescription(
  tpe: MediaType.Value
  , port: Int
  , portCount: Option[Int]
  , protocol: MediaProtocol.Value
  , format: List[Int]
  , information: Option[String]
  , connectionData: List[ConnectionData]
  , bandwidth: List[Bandwidth]
  , attributes: List[Attribute]
)



/** type of the media **/
object MediaType extends Enumeration {
  val Audio = Value("audio")
  val Video = Value("video")
  val Text = Value("text")
  val Application = Value("application")
  val Message = Value("message")
}


/** media transport protocol **/
object MediaProtocol extends Enumeration {
  val UDP = Value("udp")
  val `RTP/AVP` = Value("RTP/AVP")
  val `RTP/SAVP` = Value("RTP/SAVP")

}