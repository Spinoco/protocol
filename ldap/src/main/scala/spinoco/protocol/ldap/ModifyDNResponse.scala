package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LDAPResult

/** Response to [[ModifyDNRequest]] **/
case class ModifyDNResponse(
  response: LDAPResult
) extends ProtocolOp

object ModifyDNResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[ModifyDNResponse] =
    LDAPResult.codecInner.xmap(ModifyDNResponse(_), _.response)

}
