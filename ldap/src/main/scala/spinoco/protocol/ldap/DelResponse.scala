package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LDAPResult

case class DelResponse(
  response: LDAPResult
) extends ProtocolOp

object DelResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[DelResponse] =
    LDAPResult.codecInner.xmap(DelResponse(_), _.response)

}
