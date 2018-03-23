package spinoco.protocol.ldap

import scodec.Codec

/**
  * A response to the [[SearchRequest]], that tells the client the
  * unexplored areas of the search.
  *
  * @param uris The areas that are not yet explored, this should be URI.
  */
case class SearchResultReference(
  uris: Vector[LDAPString]
) extends ProtocolOp

object SearchResultReference {

  // Codec without the BER wrapping
  val codecInner: Codec[SearchResultReference] =
    scodec.codecs.vector(ldapString).xmap(SearchResultReference(_), _.uris)

}
