package spinoco.protocol.mgcp

import shapeless.tag.@@
import spinoco.protocol.sdp.SessionDescription

/**
  * Created by pach on 04/03/17.
  */
case class MGCPCommand(
  tpe: MGCPCommandType.Value
  , txId: Int @@ MGCPTxId
  , endpoint: LocalEndpointName
  , domain: String
  , version: MGCPVersion
  , params: List[MGCPParameter]
  , sdp: Option[SessionDescription]
)


object MGCPCommandType extends Enumeration {
  val EPCF, CRCX, MDCX, DLCX,
      RQNT, NTFY, AUEP, AUCX, RSIP = Value
}

case class MGCPVersion(major: Int, minor: Int, profile: Option[String])