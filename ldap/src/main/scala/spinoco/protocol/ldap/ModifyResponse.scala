package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LdapResult

/** Response to the [[ModifyRequest]] **/
case class ModifyResponse(
  response: LdapResult
) extends ProtocolOp

object ModifyResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[ModifyResponse] =
    LdapResult.codecInner.xmap(ModifyResponse(_), _.response)

}
