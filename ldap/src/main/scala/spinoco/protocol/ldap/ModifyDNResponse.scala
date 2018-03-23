package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LdapResult

/** Response to [[ModifyDNRequest]] **/
case class ModifyDNResponse(
  response: LdapResult
) extends ProtocolOp

object ModifyDNResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[ModifyDNResponse] =
    LdapResult.codecInner.xmap(ModifyDNResponse(_), _.response)

}
