package spinoco.protocol.stun

import java.net.{Inet4Address, InetAddress, InetSocketAddress}

import scodec.bits.ByteVector


sealed trait StunAttribute


object StunAttribute {

  /**
    * The MAPPED-ADDRESS attribute indicates a reflexive transport address
    * of the client.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.1
    *
    * @param address  Reflective address of the client
    */
  case class MappedAddress (
   address:InetSocketAddress
  ) extends StunAttribute

  /**
    * The MAPPED-ADDRESS attribute indicates a reflexive transport address
    * of the client.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.2
    *
    * @param port       Port of Reflective address of the client
    * @param address    Contains address of the client in raw, xor-ed from. address can be decoded via
    *                   decode function.
    */
  case class XorMappedAddress (
    port:Int
    , address:ByteVector
  ) extends StunAttribute  {

    /** decodes address. ipv6 requires transactionId for decoding **/
    def decode(tx:TransactionId):InetSocketAddress = {
      ???
    }

  }

  /**
    *  The USERNAME attribute is used for message integrity.  It identifies
    *  the username and password combination used in the message-integrity
    *  check.
    *
    *  https://tools.ietf.org/html/rfc5389#section-15.3
    *
    * @param authorization      The value of USERNAME is a variable-length value.  It MUST contain a
    *                           UTF-8 [RFC3629] encoded sequence of less than 513 bytes, and MUST
    *                           have been processed using SASLprep [RFC4013].
    */
  case class UserName (
    authorization:String
  ) extends StunAttribute


  /**
    * The MESSAGE-INTEGRITY attribute contains an HMAC-SHA1 [RFC2104] of
    * the STUN message.  The MESSAGE-INTEGRITY attribute can be present in
    * any STUN message type.  Since it uses the SHA1 hash, the HMAC will be
    * 20 bytes.  The text used as input to HMAC is the STUN message,
    * including the header, up to and including the attribute preceding the
    * MESSAGE-INTEGRITY attribute.  With the exception of the FINGERPRINT
    * attribute, which appears after MESSAGE-INTEGRITY, agents MUST ignore
    * all other attributes that follow MESSAGE-INTEGRITY.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.4
    *
    * @param hash   The key for the HMAC depends on whether long-term or short-term
    *               credentials are in use.
    *
    *               For long-term credentials, the key is 16 bytes:
    *
    *               key = MD5(username ":" realm ":" SASLprep(password))
    *
    *               That is, the 16-byte key is formed by taking the MD5 hash of the
    *               result of concatenating the following five fields: (1) the username,
    *               with any quotes and trailing nulls removed, as taken from the
    *               USERNAME attribute (in which case SASLprep has already been applied);
    *               (2) a single colon; (3) the realm, with any quotes and trailing nulls
    *               removed; (4) a single colon; and (5) the password, with any trailing
    *               nulls removed and after processing using SASLprep.  For example, if
    *               the username was 'user', the realm was 'realm', and the password was
    *               'pass', then the 16-byte HMAC key would be the result of performing
    *               an MD5 hash on the string 'user:realm:pass', the resulting hash being
    *               0x8493fbc53ba582fb4c044c456bdc40eb.
    *
    *               For short-term credentials:
    *
    *                 key = SASLprep(password)
    *
    *               where MD5 is defined in RFC 1321 [RFC1321] and SASLprep() is defined
    *               in RFC 4013 [RFC4013].
    *
    *
    */
  case class MessageIntegrity(
    hash:ByteVector
  )  extends StunAttribute

  /**
    * The FINGERPRINT attribute MAY be present in all STUN messages.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.5
    *
    * @param crc32    The value of the attribute is computed as the CRC-32 of the STUN message
    *                 up to (but excluding) the FINGERPRINT attribute itself, XOR'ed with
    *                 the 32-bit value 0x5354554e (the XOR helps in cases where an
    *                 application packet is also using CRC-32 in it).
    */
  case class FingerPrint(
    crc32: Long
  ) extends StunAttribute

  /**
    * The ERROR-CODE attribute is used in error response messages.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.6
    *
    * @param cause      Numeric error code value in the range of 300 to 699 plus a
    *                   textual reason phrase encoded in UTF-8 [RFC3629], and is consistent
    *                   in its code assignments and semantics with SIP [RFC3261] and HTTP
    *                   [RFC2616].
    *
    * @param phrase     The reason phrase is meant for user consumption, and can
    *                   be anything appropriate for the error code.  Recommended reason
    *                   phrases for the defined error codes are included in the IANA registry
    *                   for error codes.  The reason phrase MUST be a UTF-8 [RFC3629] encoded
    *                   sequence of less than 128 characters (which can be as long as 763
    *                   bytes).
    */
  case class ErrorCode(
    cause: ErrorCause.Value
    , phrase: String
  ) extends StunAttribute


  /**
    *  The REALM attribute may be present in requests and responses.
    *
    *  https://tools.ietf.org/html/rfc5389#section-15.7
    *
    *  @param realm     It  contains text that meets the grammar for "realm-value" as described
    *                   in RFC 3261 [RFC3261] but without the double quotes and their
    *                   surrounding whitespace.  That is, it is an unquoted realm-value (and
    *                   is therefore a sequence of qdtext or quoted-pair).  It MUST be a
    *                   UTF-8 [RFC3629] encoded sequence of less than 128 characters (which
    *                   can be as long as 763 bytes), and MUST have been processed using
    *                   SASLprep [RFC4013].
    */
  case class Realm(
    realm:String
  ) extends StunAttribute


  /**
    * The NONCE attribute may be present in requests and responses.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.8
    *
    * @param nonce      sequence of qdtext or quoted-pair, which are defined in
    *                   RFC 3261 [RFC3261].  Note that this means that the NONCE attribute
    *                   will not contain actual quote characters.  See RFC 2617 [RFC2617],
    *                   Section 4.3, for guidance on selection of nonce values in a server.
    *
    */
  case class Nonce(
    nonce:String
  ) extends StunAttribute

  /**
    * The UNKNOWN-ATTRIBUTES attribute is present only in an error response
    * when the response code in the ERROR-CODE attribute is 420.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.9
    *
    * @param attributes   The attribute contains a list of 16-bit values, each of which
    *                     represents an attribute type that was not understood by the server.
    *
    */
  case class UnknownAttributes(
    attributes:Vector[Int]
  ) extends StunAttribute

  /**
    * The SOFTWARE attribute contains a textual description of the software
    * being used by the agent sending the message.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.10
    *
    * @param description   Description of the software
    */
  case class Software(
    description:String
  ) extends StunAttribute

  /**
    * The alternate server represents an alternate transport address
    * identifying a different STUN server that the STUN client should try.
    *
    * https://tools.ietf.org/html/rfc5389#section-15.11
    *
    * @param address Address of the alternate server. Family must match the source request family.
    */
  case class AlternateServer(
    address:InetSocketAddress
  ) extends StunAttribute

  /**
    * The PRIORITY attribute indicates the priority that is to be
    * associated with a peer reflexive candidate, should one be discovered
    * by this check.
    *
    * https://tools.ietf.org/html/rfc5245#section-19.1
    *
    * @param priority   Priority of the candidate
    */
  case class Priority(
    priority:Long
  ) extends StunAttribute

  /**
    * The USE-CANDIDATE attribute indicates that the candidate pair
    * resulting from this check should be used for transmission of media.
    *
    * https://tools.ietf.org/html/rfc5245#section-19.1
    *
    */
  case object UseCandidate extends StunAttribute

  /**
    * The ICE-CONTROLLED attribute is present in a Binding request and
    * indicates that the client believes it is currently in the controlled
    * role.
    *
    * https://tools.ietf.org/html/rfc5245#section-19.1
    *
    * @param rnd  The content of the attribute is a 64-bit unsigned integer in
    *             network byte order, which contains a random number used for tie-
    *             breaking of role conflicts.
    */
  case class IceControlled(rnd:BigInt) extends StunAttribute

  /**
    * The ICE-CONTROLLING attribute is present in a Binding request and
    * indicates that the client believes it is currently in the controlling
    * role.
    *
    * https://tools.ietf.org/html/rfc5245#section-19.1
    *
    * @param rnd  The content of the attribute is a 64-bit unsigned integer in
    *             network byte order, which contains a random number used for tie-
    *             breaking of role conflicts.
    */
  case class IceControlling(rnd:BigInt) extends StunAttribute

}





sealed trait Family

object Family {

  def fromAddress(inetAddress: InetAddress) = inetAddress match {
    case _: Inet4Address => Family.IPv4
    case _: InetAddress => Family.IPv6
  }


  case object IPv4 extends Family

  case object IPv6 extends Family
}


object ErrorCause extends Enumeration {

  /**
    * Try Alternate: The client should contact an alternate server for
    * this request.  This error response MUST only be sent if the
    * request included a USERNAME attribute and a valid MESSAGE-
    * INTEGRITY attribute; otherwise, it MUST NOT be sent and error
    * code 400 (Bad Request) is suggested.  This error response MUST
    * be protected with the MESSAGE-INTEGRITY attribute, and receivers
    * MUST validate the MESSAGE-INTEGRITY of this response before
    * redirecting themselves to an alternate server.
    *
    * Note: Failure to generate and validate message integrity
    *       for a 300 response allows an on-path attacker to falsify a
    *       300 response thus causing subsequent STUN messages to be
    *       sent to a victim.
    */
  val TryAlternate = Value(300)

  /**
    * Bad Request: The request was malformed.  The client SHOULD NOT
    * retry the request without modification from the previous
    * attempt.  The server may not be able to generate a valid
    * MESSAGE-INTEGRITY for this error, so the client MUST NOT expect
    * a valid MESSAGE-INTEGRITY attribute on this response.
    */
  val BadRequest = Value(400)

  /**
    * Unauthorized: The request did not contain the correct
    * credentials to proceed.  The client should retry the request
    * with proper credentials.
    */
  val Unauthorized = Value(401)

  /**
    * Unknown Attribute: The server received a STUN packet containing
    * a comprehension-required attribute that it did not understand.
    * The server MUST put this unknown attribute in the UNKNOWN-
    * ATTRIBUTE attribute of its error response.
    */
  val UnknownAttribute = Value(420)

  /**
    * Stale Nonce: The NONCE used by the client was no longer valid.
    * The client should retry, using the NONCE provided in the
    * response.
    */
  val StaleNonce = Value(438)

  /**
    * Server Error: The server has suffered a temporary error.  The
    * client should try again.
    */
  val ServerError = Value(500)
}