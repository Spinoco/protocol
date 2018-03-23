package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LDAPResult

/** Response to the [[ModifyRequest]] **/
case class ModifyResponse(
  response: LDAPResult
) extends ProtocolOp

object ModifyResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[ModifyResponse] =
    LDAPResult.codecInner.xmap(ModifyResponse(_), _.response)

}
