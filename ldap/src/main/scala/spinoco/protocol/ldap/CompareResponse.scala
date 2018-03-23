package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LdapResult

/** Response to the [[CompareRequest]] **/
case class CompareResponse(
  response: LdapResult
) extends ProtocolOp

object CompareResponse {

  // Codec without the BER wrapping
  val codecInner: Codec[CompareResponse] =
    LdapResult.codecInner.xmap(CompareResponse(_), _.response)

}
