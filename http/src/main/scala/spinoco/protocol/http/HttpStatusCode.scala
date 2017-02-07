package spinoco.protocol.http

import scodec.{Attempt, Codec, Err}
import spinoco.protocol.http.HttpStatusCode.Success


sealed trait HttpStatusCode { self =>
  def code: Int
  def label: String
  def description : String
  def longDescription : String

  def isSuccess: Boolean = self match {
    case _: Success => true
    case _ => false
  }
}





/**
  * Created by pach on 05/01/17.
  */
object HttpStatusCode {

  sealed case class Progress(code: Int, label: String, description: String, longDescription: String = "") extends HttpStatusCode

  sealed case class Success(code: Int, label: String, description: String, longDescription: String = "") extends HttpStatusCode

  sealed case class Redirect(code: Int, label: String, description: String, longDescription: String = "") extends HttpStatusCode

  sealed case class ClientError(code: Int, label: String, description: String, longDescription: String = "") extends HttpStatusCode

  sealed case class ServerError(code: Int, label: String, description: String, longDescription: String = "") extends HttpStatusCode

  val Continue = Progress(100, "Continue", "The server has received the request headers, and the client should proceed to send the request body.")
  val SwitchingProtocols = Progress(101, "Switching Protocols", "The server is switching protocols, because the client requested the switch.")
  val Processing = Progress(102, "Processing", "The server is processing the request, but no response is available yet.")

  val Ok = Success(200, "OK", "OK")
  val Created = Success(201, "Created", "The request has been fulfilled and resulted in a new resource being created.")
  val Accepted = Success(202, "Accepted", "The request has been accepted for processing, but the processing has not been completed.")
  val NonAuthoritativeInformation = Success(203, "Non-Authoritative Information", "The server successfully processed the request, but is returning information that may be from another source.")
  val NoContent = Success(204, "No Content", "", "")
  val ResetContent = Success(205, "Reset Content", "The server successfully processed the request, but is not returning any content.")
  val PartialContent = Success(206, "Partial Content", "The server is delivering only part of the resource due to a range header sent by the client.")
  val MultiStatus = Success(207, "Multi-Status", "The message body that follows is an XML message and can contain a number of separate response codes, depending on how many sub-requests were made.")
  val AlreadyReported = Success(208, "Already Reported", "The members of a DAV binding have already been enumerated in a previous reply to this request, and are not being included again.")
  val IMUsed = Success(226, "IM Used", "The server has fulfilled a GET request for the resource, and the response is a representation of the result of one or more instance-manipulations applied to the current instance.")
  val MultipleChoices = Redirect(300, "Multiple Choices", "There are multiple options for the resource that the client may follow.", "There are multiple options for the resource that the client may follow. The preferred one is <a href=\"%s\">this URI</a>.")
  val MovedPermanently = Redirect(301, "Moved Permanently", "This and all future requests should be directed to the given URI.", "This and all future requests should be directed to <a href=\"%s\">this URI</a>.")
  val Found = Redirect(302, "Found", "The resource was found, but at a different URI.", "The requested resource temporarily resides under <a href=\"%s\">this URI</a>.")
  val SeeOther = Redirect(303, "See Other", "The response to the request can be found under another URI using a GET method.", "The response to the request can be found under <a href=\"%s\">this URI</a> using a GET method.")
  val NotModified = Redirect(304, "Not Modified", "The resource has not been modified since last requested.", "")
  val UseProxy = Redirect(305, "Use Proxy", "This single request is to be repeated via the proxy given by the Location field.", "This single request is to be repeated via the proxy under <a href=\"%s\">this URI</a>.")
  val TemporaryRedirect = Redirect(307, "Temporary Redirect", "The request should be repeated with another URI, but future requests can still use the original URI.", "The request should be repeated with <a href=\"%s\">this URI</a>, but future requests can still use the original URI.")
  val PermanentRedirect = Redirect(308, "Permanent Redirect", "The request, and all future requests should be repeated using another URI.", "The request, and all future requests should be repeated using <a href=\"%s\">this URI</a>.")

  val BadRequest = ClientError(400, "Bad Request", "The request contains bad syntax or cannot be fulfilled.")
  val Unauthorized = ClientError(401, "Unauthorized", "Authentication is possible but has failed or not yet been provided.")
  val PaymentRequired = ClientError(402, "Payment Required", "Reserved for future use.")
  val Forbidden = ClientError(403, "Forbidden", "The request was a legal request, but the server is refusing to respond to it.")
  val NotFound = ClientError(404, "Not Found", "The requested resource could not be found but may be available again in the future.")
  val MethodNotAllowed = ClientError(405, "Method Not Allowed", "A request was made of a resource using a request method not supported by that resource;")
  val NotAcceptable = ClientError(406, "Not Acceptable", "The requested resource is only capable of generating content not acceptable according to the Accept headers sent in the request.")
  val ProxyAuthenticationRequired = ClientError(407, "Proxy Authentication Required", "Proxy authentication is required to access the requested resource.")
  val RequestTimeout = ClientError(408, "Request Timeout", "The server timed out waiting for the request.")
  val Conflict = ClientError(409, "Conflict", "The request could not be processed because of conflict in the request, such as an edit conflict.")
  val Gone = ClientError(410, "Gone", "The resource requested is no longer available and will not be available again.")
  val LengthRequired = ClientError(411, "Length Required", "The request did not specify the length of its content, which is required by the requested resource.")
  val PreconditionFailed = ClientError(412, "Precondition Failed", "The server does not meet one of the preconditions that the requester put on the request.")
  val RequestEntityTooLarge = ClientError(413, "Request Entity Too Large", "The request is larger than the server is willing or able to process.")
  val RequestUriTooLong = ClientError(414, "Request-URI Too Long", "The URI provided was too long for the server to process.")
  val UnsupportedMediaType = ClientError(415, "Unsupported Media Type", "The request entity has a media type which the server or resource does not support.")
  val RequestedRangeNotSatisfiable = ClientError(416, "Requested Range Not Satisfiable", "The client has asked for a portion of the file, but the server cannot supply that portion.")
  val ExpectationFailed = ClientError(417, "Expectation Failed", "The server cannot meet the requirements of the Expect request-header field.")
  val EnhanceYourCalm = ClientError(420, "Enhance Your Calm", "You are being rate-limited.")
  val UnprocessableEntity = ClientError(422, "Unprocessable Entity", "The request was well-formed but was unable to be followed due to semantic errors.")
  val Locked = ClientError(423, "Locked", "The resource that is being accessed is locked.")
  val FailedDependency = ClientError(424, "Failed Dependency", "The request failed due to failure of a previous request.")
  val UnorderedCollection = ClientError(425, "Unordered Collection", "The collection is unordered.")
  val UpgradeRequired = ClientError(426, "Upgrade Required", "The client should switch to a different protocol.")
  val PreconditionRequired = ClientError(428, "Precondition Required", "The server requires the request to be conditional.")
  val TooManyRequests = ClientError(429, "Too Many Requests", "The user has sent too many requests in a given amount of time.")
  val RequestHeaderFieldsTooLarge = ClientError(431, "Request Header Fields Too Large", "The server is unwilling to process the request because either an individual header field, or all the header fields collectively, are too large.")
  val RetryWith = ClientError(449, "Retry With", "The request should be retried after doing the appropriate action.")
  val BlockedByParentalControls = ClientError(450, "Blocked by Windows Parental Controls", "Windows Parental Controls are turned on and are blocking access to the given webpage.")
  val UnavailableForLegalReasons = ClientError(451, "Unavailable For Legal Reasons", "Resource access is denied for legal reasons.")

  val InternalServerError = ServerError(500, "Internal Server Error", "There was an internal server error.")
  val NotImplemented = ServerError(501, "Not Implemented", "The server either does not recognize the request method, or it lacks the ability to fulfill the request.")
  val BadGateway = ServerError(502, "Bad Gateway", "The server was acting as a gateway or proxy and received an invalid response from the upstream server.")
  val ServiceUnavailable = ServerError(503, "Service Unavailable", "The server is currently unavailable (because it is overloaded or down for maintenance).")
  val GatewayTimeout = ServerError(504, "Gateway Timeout", "The server was acting as a gateway or proxy and did not receive a timely request from the upstream server.")
  val HTTPVersionNotSupported = ServerError(505, "HTTP Version Not Supported", "The server does not support the HTTP protocol version used in the request.")
  val VariantAlsoNegotiates = ServerError(506, "Variant Also Negotiates", "Transparent content negotiation for the request, results in a circular reference.")
  val InsufficientStorage = ServerError(507, "Insufficient Storage", "Insufficient storage to complete the request.")
  val LoopDetected = ServerError(508, "Loop Detected", "The server detected an infinite loop while processing the request.")
  val BandwidthLimitExceeded = ServerError(509, "Bandwidth Limit Exceeded", "Bandwidth limit has been exceeded.")
  val NotExtended = ServerError(510, "Not Extended", "Further extensions to the request are required for the server to fulfill it.")
  val NetworkAuthenticationRequired = ServerError(511, "Network Authentication Required", "The client needs to authenticate to gain network access.")
  val NetworkReadTimeout = ServerError(598, "Network read timeout error", "")
  val NetworkConnectTimeout = ServerError(599, "Network connect timeout error", "")


  val allCodes = Seq(
    Continue
    , SwitchingProtocols
    , Processing
    , Ok
    , Created
    , Accepted
    , NonAuthoritativeInformation
    , NoContent
    , ResetContent
    , PartialContent
    , MultiStatus
    , AlreadyReported
    , IMUsed
    , MultipleChoices
    , MovedPermanently
    , Found
    , SeeOther
    , NotModified
    , UseProxy
    , TemporaryRedirect
    , PermanentRedirect
    , BadRequest
    , Unauthorized
    , PaymentRequired
    , Forbidden
    , NotFound
    , MethodNotAllowed
    , NotAcceptable
    , ProxyAuthenticationRequired
    , RequestTimeout
    , Conflict
    , Gone
    , LengthRequired
    , PreconditionFailed
    , RequestEntityTooLarge
    , RequestUriTooLong
    , UnsupportedMediaType
    , RequestedRangeNotSatisfiable
    , ExpectationFailed
    , EnhanceYourCalm
    , UnprocessableEntity
    , Locked
    , FailedDependency
    , UnorderedCollection
    , UpgradeRequired
    , PreconditionRequired
    , TooManyRequests
    , RequestHeaderFieldsTooLarge
    , RetryWith
    , BlockedByParentalControls
    , UnavailableForLegalReasons
    , InternalServerError
    , NotImplemented
    , BadGateway
    , ServiceUnavailable
    , GatewayTimeout
    , HTTPVersionNotSupported
    , VariantAlsoNegotiates
    , InsufficientStorage
    , LoopDetected
    , BandwidthLimitExceeded
    , NotExtended
    , NetworkAuthenticationRequired
    , NetworkReadTimeout
    , NetworkConnectTimeout
  ).map { c => c.code -> c }.toMap


  def fromCode(code: Int): Option[HttpStatusCode] =
    allCodes.get(code)


  val codec: Codec[HttpStatusCode] = {
    import spinoco.protocol.common.codec._
    intAsString.exmap(
      i => allCodes.get(i).map(Attempt.successful).getOrElse(Attempt.failure(Err(s"Unsupported code $i")))
      , c => Attempt.successful(c.code)
    )

  }

}