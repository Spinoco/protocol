package spinoco.protocol.mgcp

import shapeless.tag.@@
import spinoco.protocol.sdp.SessionDescription

/**
  * MGCP Command Response
  */
case class MGCPResponse(
  code            : MGCPResponseCode
  , txId          : Int @@ MGCPTxId
  , packageName   : Option[String]
  , responseString: Option[String]
  , params        : List[MGCPParameter]
  , sdp           : Option[SessionDescription]
)
