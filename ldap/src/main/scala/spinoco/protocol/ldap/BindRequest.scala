package spinoco.protocol.ldap

import scodec.Codec
import scodec.codecs
import scodec.bits.ByteVector
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.{BerClass, Identifier}
import spinoco.protocol.ldap.BindRequest.AuthenticationChoice
import spinoco.protocol.ldap.elements.LdapDN
import spinoco.protocol.common.codec._


/**
  * A request for authentication of the client to the server.
  *
  * @param version  The version of the protocol.
  * @param name     The name of the directory object the client wishes to bind as.
  * @param auth     The method of authentication against the server.
  */
case class BindRequest(
  version: Int
  , name: LdapDN
  , auth: AuthenticationChoice
) extends ProtocolOp

object BindRequest {

  case class Simple(pw: ByteVector) extends AuthenticationChoice

  case class SaslCredentials(
    mechanism: String
    , credentials: Option[ByteVector]
  ) extends AuthenticationChoice

  sealed trait AuthenticationChoice

  val simpleAuthCodec: Codec[Simple] = codecs.bytes.xmap(Simple, _.pw)

  val saslAuthCodec: Codec[SaslCredentials] = ber.sequence{
    (ldapString :: maybe(ber.octetStringPrimitive) ).as[SaslCredentials]
  }

  val authenticationCodec: Codec[AuthenticationChoice] =
    ber.discriminated[AuthenticationChoice]
    .typecase(Identifier(BerClass.Context, false, 0), ber.finiteLength(simpleAuthCodec))
    .typecase(Identifier(BerClass.Context, false, 3), ber.finiteLength(saslAuthCodec))

  // Codec without the BER wrapping
  val codecInner: Codec[BindRequest] =
    (intBounded(ber.integer)(1, 127) :: LdapDN.codec :: authenticationCodec).as[BindRequest]

}
