package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LdapResult

/** Response to the [[AddRequest]] **/
case class AddResponse(
  response: LdapResult
) extends ProtocolOp

object AddResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[AddResponse] =
    LdapResult.codecInner.xmap(AddResponse(_), _.response)

}
