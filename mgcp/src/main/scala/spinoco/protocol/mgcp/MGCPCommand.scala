package spinoco.protocol.mgcp

import shapeless.tag.@@
import spinoco.protocol.sdp.SessionDescription

/**
  * Reperesents MGCP command between gateway and CA (Call Agent)
  * @param tpe          Type of the command
  * @param txId         A transaction identifier
  * @param endpoint     Name of the mgcp endpoint
  * @param domain       Domain of the endpoint
  * @param version      MGCP Protocol version
  * @param params       Any parameters
  * @param sdp          If provided, SDP sent with the command
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
  val
  EPCF      // EndpointConfiguration https://tools.ietf.org/html/rfc3435#section-2.3.2
  , CRCX    // CreateConnection https://tools.ietf.org/html/rfc3435#section-2.3.5
  , MDCX    // ModifyConnection https://tools.ietf.org/html/rfc3435#section-2.3.6
  , DLCX    // DeleteConnection https://tools.ietf.org/html/rfc3435#section-2.3.7, https://tools.ietf.org/html/rfc3435#section-2.3.8, https://tools.ietf.org/html/rfc3435#section-2.3.9
  , RQNT    // NotificationRequest https://tools.ietf.org/html/rfc3435#section-2.3.3
  , NTFY    // Notify https://tools.ietf.org/html/rfc3435#section-2.3.4
  , AUEP    // AuditEndpoint  https://tools.ietf.org/html/rfc3435#section-2.3.10
  , AUCX    // AuditConnection  https://tools.ietf.org/html/rfc3435#section-2.3.11
  , RSIP    // RestartInProgress  https://tools.ietf.org/html/rfc3435#section-2.3.12
  = Value
}

case class MGCPVersion(major: Int, minor: Int, profile: Option[String])

object MGCPVersion {

  // MGCP V1 version placeholder
  val V1 = MGCPVersion(1, 0, None)

}