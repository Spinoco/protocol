package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LDAPResult

/** Response to the [[AddRequest]] **/
case class AddResponse(
  response: LDAPResult
) extends ProtocolOp

object AddResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[AddResponse] =
    LDAPResult.codecInner.xmap(AddResponse(_), _.response)

}
