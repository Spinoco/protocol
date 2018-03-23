package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LDAPResult

/** Response to the [[CompareRequest]] **/
case class CompareResponse(
  response: LDAPResult
) extends ProtocolOp

object CompareResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[CompareResponse] =
    LDAPResult.codecInner.xmap(CompareResponse(_), _.response)

}
