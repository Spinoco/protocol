package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.ldap.elements.LDAPResult

/**
  * A search request is completed, no more entries to follow.
  *
  * @param result The status how the request ended.
  */
case class SearchResultDone(
  result: LDAPResult
) extends ProtocolOp

object SearchResultDone {

  // Codec without the BER wrapping
  val codecInner: Codec[SearchResultDone] =
    LDAPResult.codecInner.xmap(SearchResultDone(_), _.result)

}
