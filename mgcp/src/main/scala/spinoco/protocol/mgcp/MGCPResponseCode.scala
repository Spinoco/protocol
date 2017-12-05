package spinoco.protocol.mgcp

import spinoco.protocol.mgcp.MGCPResponseCode.{Provisional, Success}


object MGCPResponseCode {

  case class Success(code:Int) extends MGCPResponseCode
  case class Provisional(code: Int) extends MGCPResponseCode
  case class Error(code: Int) extends MGCPResponseCode
  case class PackageSpecific(code: Int) extends MGCPResponseCode

  // Acknowledgement of response
  val Ack = Success(0)

  // The transaction is currently being executed.  An actual
  // completion message will follow later.
  val InProgress = Provisional(100)

  // The transaction has been queued for execution.  An actual
  // completion message will follow later.
  val Queued = Provisional(101)

  // The requested transaction was executed normally.  This return
  // code can be used for a successful response to any command.
  val Ok = Success(200)

  // The connection was deleted.  This return code can only be used
  // for a successful response to a DeleteConnection command.
  val DeletedSuccessfully = Success(250)

  // The transaction could not be executed, due to some unspecified
  // transient error.
  val TransientError = Error(400)

  //The phone is already off hook.
  val AlreadyOffHook = Error(401)

  // The phone is already on hook.
  val AlreadyOnHook = Error(402)

  // The transaction could not be executed, because the endpoint does
  // not have sufficient resources at this time.
  val InsufficientResources = Error(403)

  // Insufficient bandwidth at this time.
  val InsufficientBandwidth = Error(404)

  // The transaction could not be executed, because the endpoint is "restarting".
  val RestartInProgress = Error(405)

  // Transaction time-out.  The transaction did not complete in a
  // reasonable period of time and has been aborted.
  val TransactionTimeout = Error(406)

  // Transaction aborted.  The transaction was aborted by some
  // external action, e.g., a ModifyConnection command aborted by a
  // DeleteConnection command.
  val TransactionAborted = Error(407)

  // The transaction could not be executed because of internal
  // overload.
  val SystemOverloaded = Error(409)

  // No endpoint available.  A valid "any of" wildcard was used,
  // however there was no endpoint available to satisfy the request.
  val NoEndpointAvailable = Error(410)

  // The transaction could not be executed, because the endpoint is
  // unknown.
  val EndpointNotFound = Error(500)

  // The transaction could not be executed, because the endpoint is
  // not ready.  This includes the case where the endpoint is out-of-
  // service.
  val NotReady = Error(501)

  // The transaction could not be executed, because the endpoint does
  // not have sufficient resources (permanent condition).
  val NoResources = Error(502)

  // "All of" wildcard too complicated.
  val WildcardTooComplicated = Error(503)

  // 504 Unknown or unsupported command.
  val Unsupported  = Error(504)

  // Unsupported RemoteConnectionDescriptor.  This SHOULD be used when
  // one or more mandatory parameters or values in the
  // RemoteConnectionDescriptor is not supported.
  val UnsupportedRemoteDescriptor = Error(505)


  // Unable to satisfy both LocalConnectionOptions and
  // RemoteConnectionDescriptor.  This SHOULD be used when the
  // LocalConnectionOptions and RemoteConnectionDescriptor contain one
  // or more mandatory parameters or values that conflict with each
  // other and/or cannot be supported at the same time (except for
  // codec negotiation failure - see error code 534).
  val ConflictingDescriptor = Error(505)


  // Unsupported functionality. Some unspecified functionality
  // required to carry out the command is not supported. Note that
  // several other error codes have been defined for specific areas of
  // unsupported functionality (e.g. 508, 511, etc.), and this error
  // code SHOULD only be used if there is no other more specific error
  // code for the unsupported functionality.
  val UnsupportedFunctionality = Error(507)

  // Unknown or unsupported quarantine handling.
  val UnsupportedQuarantine = Error(508)

  // Error in RemoteConnectionDescriptor.  This SHOULD be used when
  // there is a syntax or semantic error in the
  // RemoteConnectionDescriptor.
  val ErrorInRemoteDescriptor = Error(509)

  // The transaction could not be executed, because some unspecified
  // protocol error was detected.  Automatic recovery from such an
  // error will be very difficult, and hence this code SHOULD only be
  // used as a last resort.
  val UnspecifiedProtocol = Error(510)

  // The transaction could not be executed, because the command
  // contained an unrecognized extension.  This code SHOULD be used
  // for unsupported critical parameter extensions ("X+").
  val UnknownExtension = Error(510)

  // The transaction could not be executed, because the gateway is not
  // equipped to detect one of the requested events.
  val UnsupportedEvent = Error(512)

  // The transaction could not be executed, because the gateway is not
  // equipped to generate one of the requested signals.
  val UnsupportedSignal = Error(512)

  // The transaction could not be executed, because the gateway cannot
  // send the specified announcement.
  val UnsupportedAnnouncement = Error(514)

  // The transaction refers to an incorrect connection-id (may have
  // been already deleted).
  val UnknownConnectionId = Error(515)

  // The transaction refers to an unknown call-id, or the call-id
  // supplied is incorrect (e.g., connection-id not associated with
  // this call-id).
  val UnknownCallId = Error(516)

  // Unsupported or invalid mode.
  val UnsupportedMode = Error(517)

  // Unsupported or unknown package.  It is RECOMMENDED to include a
  // PackageList parameter with the list of supported packages in the
  // response, especially if the response is generated by the Call
  // Agent.
  val UnknownPackage = Error(518)

  // Endpoint does not have a digit map.
  val UnsupportedDigitMap = Error(519)

  // The transaction could not be executed, because the endpoint is
  // "restarting".  In most cases this would be a transient error, in
  // which case, error code 405 SHOULD be used instead.  The error
  // code is only included here for backwards compatibility.
  val EndpointRestarting = Error(520)

  // Endpoint redirected to another Call Agent.  The associated
  // redirection behavior is only well-defined when this response is
  // issued for a RestartInProgress command.
  val RedirectedToOtherAgent = Error(521)


  // No such event or signal.  The request referred to an event or
  // signal that is not defined in the relevant package (which could
  //  be the default package).
  val UnknownEventOrSignal = Error(522)

  // Unknown action or illegal combination of actions.
  val UnsupportedAction = Error(523)

  // Internal inconsistency in LocalConnectionOptions.
  val InternalIncosistency = Error(524)

  // Unknown extension in LocalConnectionOptions.  This code SHOULD be
  // used for unsupported mandatory vendor extensions ("x+").
  val UnknonwExtension = Error(525)

  // Insufficient bandwidth.  In cases where this is a transient
  // error, error code 404 SHOULD be used instead.
  val InsufficientBandwidthPermanent = Error(526)

  // Missing RemoteConnectionDescriptor.
  val MissingRemoteDescriptor = Error(527)

  // Incompatible protocol version.
  val IncompatibleVersion = Error(528)

  // Internal hardware failure.
  val HardwareFailure = Error(529)

  // CAS signaling protocol error.
  val CASSignallingError = Error(530)

  // Failure of a grouping of trunks (e.g., facility failure).
  val TrunkFailure = Error(531)

  // Unsupported value(s) in LocalConnectionOptions.
  val UnsupportedValue = Error(532)

  // Response too large.
  val ResponseTooLarge = Error(533)

  // Codec negotiation failure.
  val CodecNegotiationFailure = Error(534)

  // Packetization period not supported.
  val UnsupportedPacketizationPeriod = Error(535)

  // Unknown or unsupported RestartMethod.
  val UnsupportedRestartMethod = Error(536)

  // Unknown or unsupported digit map extension.
  val UnsupportedDigitMapExtension = Error(537)

  // Event/signal parameter error (e.g., missing, erroneous,
  // unsupported, unknown, etc.).
  val EventOrSignalParameterError = Error(538)

  // Invalid or unsupported command parameter. This code SHOULD only
  // be used when the parameter is neither a package or vendor
  // extension parameter.
  val UnsupportedCommandParameter = Error(539)

  // Per endpoint connection limit exceeded.
  val ConnectionLimitExceeded = Error(540)

  // Invalid or unsupported LocalConnectionOptions. This code SHOULD
  // only be used when the LocalConnectionOptions is neither a package
  // nor a vendor extension LocalConnectionOptions.
  val UnsupportedLocalConnectionOption = Error(541)

}



sealed trait  MGCPResponseCode { self =>
  val code: Int
  /** yields true if the code is provisional **/
  def isProvisional: Boolean = self.isInstanceOf[Provisional]
  def isSuccess: Boolean = self.isInstanceOf[Success]
}
