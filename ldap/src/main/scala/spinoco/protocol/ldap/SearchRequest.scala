package spinoco.protocol.ldap

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.common
import spinoco.protocol.ldap.SearchRequest.{Aliases, SearchScope}
import spinoco.protocol.ldap.elements.{AttributeSelector, Filter, LDAPDN}

/**
  * Request to perform a LDAP search.
  *
  * @param baseObject     The name of the base object entry.
  * @param scope          The scope of the search.
  * @param deferAliases   An indicator as to whether or not alias entries are to be
  *                       dereferenced during stages of the Search operation.
  *
  * @param sizeLimit      The number of entries to be returned
  * @param timeLimit      The time limit for the search in seconds
  * @param typesOnly      Whether the result should include only the attribute descriptions (false)
  *                       or if we want to return the values to the descriptions as well (true).
  * @param filter         A filter that defines the conditions that must be fulfilled in order for the search to be matched.
  * @param attributes     A selection of attributes to be returned for search entries that match this search.
  */
case class SearchRequest(
  baseObject: LDAPDN
  , scope: SearchScope.Value
  , deferAliases: Aliases.Value
  , sizeLimit: Int
  , timeLimit: Int
  , typesOnly: Boolean
  , filter: Filter
  , attributes: Vector[AttributeSelector]
) extends ProtocolOp

object SearchRequest {


  object SearchScope extends Enumeration {
    // The scope is constrained to the entry named by baseObject.
    val baseObject = Value(0)

    // he scope is constrained to the immediate subordinates of the entry named by baseObject.
    val singleLevel = Value(1)

    // The scope is constrained to the entry named by baseObject and to all its subordinates
    val wholeSubtree = Value(2)
  }

  object Aliases extends Enumeration {
    // Do not dereference aliases in searching or in locating the base object of the Search
    val neverDerefAliases = Value(0)

    // While searching subordinates of the base object, dereference any alias within the search scope.
    val derefInSearching = Value(1)

    // Dereference aliases in locating the base object of the Search,
    // but not when searching subordinates of the base object.
    val derefFindingBaseObj = Value(2)

    // Dereference aliases both in searching and in locating the base object of the Search.
    val derefAlways = Value(3)
  }

  // Codec without the BER wrapping
  val codecInner: Codec[SearchRequest] =
    (LDAPDN.codec ::
     scodec.codecs.enumerated(ber.enumerated, SearchScope) ::
     scodec.codecs.enumerated(ber.enumerated, Aliases) ::
     common.codec.intBounded(ber.integer)(0, Int.MaxValue) ::
     common.codec.intBounded(ber.integer)(0, Int.MaxValue) ::
     boolean ::
     Filter.codec ::
     AttributeSelector.selectionCodec
    ).as[SearchRequest]

}
