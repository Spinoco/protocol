package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LdapResult

/**
  * A search request is completed, no more entries to follow.
  *
  * @param result The status how the request ended.
  */
case class SearchResultDone(
  result: LdapResult
) extends ProtocolOp

object SearchResultDone {

  // Codec without the BER wrapping
  val codecInner: Codec[SearchResultDone] =
    LdapResult.codecInner.xmap(SearchResultDone(_), _.result)

}
