package spinoco.protocol.sdp

import java.net.URI

/**
  * Description of the session
  *
  * @param version            Version of the session, currently 0
  * @param origin             Originator and session identifier
  * @param name               Name of the session
  * @param information        Additional session information
  * @param uri                Uri containing further session information
  * @param email              Email(s) of the session originator
  * @param phone              Phone(s) of the session originator
  * @param connectionData     Information  about connetcion. Either this must be specified or
  *                           each media description must have its connection data specified.
  * @param bandwidth          Session Bandwidth parameteres
  * @param timing             Time Description
  * @param repeat             Time repeat information
  * @param zones              Time zones
  * @param attributes         Attributes of the session
  * @param media              Media Descriptions
  */
case class SessionDescription(
  version: Int
  , origin: SessionOrigin
  , name: String
  , information: Option[String]
  , uri: Option[URI]
  , email: List[String]
  , phone: List[String]
  , connectionData: List[ConnectionData]
  , bandwidth: List[Bandwidth]
  , timing: List[Timing]
  , repeat: List[Repeat]
  , zones: List[TimeZone]
  , attributes: List[Attribute]
  , media: List[MediaDescription]
)


object SessionDescription {

  def apply(
     version: Int
     , origin: SessionOrigin
     , name: String
  ): SessionDescription = {
    SessionDescription(
      version = version
      , origin = origin
      , name = name
      , information = None
      , uri = None
      , email = Nil
      , phone = Nil
      , connectionData = Nil
      , bandwidth = Nil
      , timing = Nil
      , repeat = Nil
      , zones = Nil
      , attributes = Nil
      , media = Nil
    )
  }

}