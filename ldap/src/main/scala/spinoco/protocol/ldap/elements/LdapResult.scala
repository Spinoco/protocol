package spinoco.protocol.ldap.elements

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.ldap.elements.LdapResult.ResultCode
import spinoco.protocol.ldap._
import spinoco.protocol.common.codec.{maybe, minItems}


/**
  * The result of an LDAP request.
  *
  * @param resultCode        The code for the result of the operation.
  * @param matchedDN         In most cases this field is to be empty except for cases
  *                          specified in the: RFC 4511 section 4.1.9.
  * @param diagnosticMessage Human readable message for the result. Non standardised,
  *                          implementation has to be independent of this.
  * @param referral          The list of URIs that can be accessed via LDAP to fulfil the request.
  *                          This field exists only in case the result code is set to [[spinoco.protocol.ldap.elements.LdapResult.ResultCode.referral]].
  */
case class LdapResult(
  resultCode: ResultCode.Value
  , matchedDN: LdapDN
  , diagnosticMessage: LDAPString
  , referral: Option[Vector[LDAPString]]
)

object LdapResult {

  val referralCodec: Codec[Vector[LDAPString]] =
    ber.sequence(minItems(1)(scodec.codecs.vector(ldapString)))

  // Codec without the BER wrapping
  val codecInner: Codec[LdapResult] =
    (scodec.codecs.enumerated(ber.enumerated, ResultCode) ::
      LdapDN.codec ::
      ldapString ::
      maybe(referralCodec))
    .as[LdapResult]


  // For explanation what given code means, refer to: https://tools.ietf.org/html/rfc4511#appendix-A
  object ResultCode extends Enumeration {

    val success = Value(0)
    val operationsError = Value(1)
    val protocolError = Value(2)
    val timeLimitExceeded = Value(3)
    val sizeLimitExceeded = Value(4)
    val compareFalse = Value(5)
    val compareTrue = Value(6)
    val authMethodNotSupported = Value(7)
    val strongerAuthRequired = Value(8)

    //9 reserved

    val referral = Value(10)
    val adminLimitExceeded = Value(11)
    val unavailableCriticalExtension = Value(12)
    val confidentialityRequired = Value(13)
    val saslBindInProgress = Value(14)

    //15 not in RFC

    val noSuchAttribute = Value(16)
    val undefinedAttributeType = Value(17)
    val inappropriateMatching = Value(18)
    val constraintViolation = Value(19)
    val attributeOrValueExists = Value(20)
    val invalidAttributeSyntax = Value(21)

    // 22 - 31 unused

    val noSuchObject = Value(32)
    val aliasProblem = Value(33)
    val invalidDNSyntax = Value(34)

    // 35 reserved for undefined isLeaf

    val aliasDereferencingProblem = Value(36)

    // 37 - 47 unused

    val inappropriateAuthentication = Value(48)
    val invalidCredentials = Value(49)
    val insufficientAccessRights = Value(50)
    val busy = Value(51)
    val unavailable = Value(52)
    val unwillingToPerform = Value(53)
    val loopDetect = Value(54)

    // 55 - 63 unused

    val namingViolation = Value(64)
    val objectClassViolation = Value(65)
    val notAllowedOnNonLeaf = Value(66)
    val notAllowedOnRDN = Value(67)
    val entryAlreadyExists = Value(68)
    val objectClassModsProhibited = Value(69)

    // 70 reserved for CLDAP

    val affectsMultipleDSAs = Value(71)

    // 72 - 79 unused

    val other = Value(80)

  }

}
