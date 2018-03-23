package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LdapResult

case class DelResponse(
  response: LdapResult
) extends ProtocolOp

object DelResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[DelResponse] =
    LdapResult.codecInner.xmap(DelResponse(_), _.response)

}
