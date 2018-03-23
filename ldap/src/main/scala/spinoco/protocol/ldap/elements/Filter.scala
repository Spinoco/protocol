package spinoco.protocol.ldap.elements

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.{ClassTag, Identifier}
import spinoco.protocol.common

/** Filter for [[spinoco.protocol.ldap.SearchRequest]] **/
sealed trait Filter

object Filter {

  /**
    * Filter that puts the provided filters in AND relationship.
    *
    * @param filters  The filters that all have to result to true in order to pass the filter query.
    */
  case class And(
    filters: Set[Filter]
  ) extends Filter

  // Codec without the BER wrapping
  val andCodecInner: Codec[And] =
    scodec.codecs.lazily(common.codec.minItems(1)(common.codec.set(codec)).xmap(And, _.filters))

  /**
    * Filter that puts the provided filters in OR relationship.
    *
    * @param filters  Filters where at least one has to result to true in order to pass the filter query.
    */
  case class Or(
    filters: Set[Filter]
  ) extends Filter

  // Codec without the BER wrapping
  val orCodecInner: Codec[Or] =
    scodec.codecs.lazily(common.codec.minItems(1)(common.codec.set(codec)).xmap(Or, _.filters))

  /**
    * Filter that puts the provided filter into NOT relationship.
    *
    * @param filter The filter that has to result into false in order to pass the filter query.
    */
  case class Not(
    filter: Filter
  ) extends Filter

  // Codec without the BER wrapping
  val notCodecInner: Codec[Not] =
    scodec.codecs.lazily(codec.xmap(Not, _.filter))

  /**
    * Filter that checks the equality of the provided attribute.
    *
    * @param attribute  The attribute has to be equal with the object's attribute in order to pass this query.
    */
  case class EqualityMatch(
    attribute: AttributeValueAssertion
  ) extends Filter

  // Codec without the BER wrapping
  val equalityCodecInner: Codec[EqualityMatch] =
    AttributeValueAssertion.codecInner.xmap(EqualityMatch, _.attribute)

  /**
    * Check that a given attribute matches to a given substring values.
    *
    * @param tpe        The attribute that has to match to the substrings.
    * @param subStrings The substrings that have to match to the attribute.
    */
  case class SubstringFilter(
    tpe: AttributeDescription
    , subStrings: SubStrings
  ) extends Filter

  // Codec without the BER wrapping
  val substringCodecInner: Codec[SubstringFilter] =
    (AttributeDescription.codec ::
      SubStrings.codec).as[SubstringFilter]

  /**
    * Check that the given attribute is grater or equal to the provided value.
    *
    * @param attribute  The attribute to be check.
    */
  case class GreaterOrEqual(
    attribute: AttributeValueAssertion
  ) extends Filter

  // Codec without the BER wrapping
  val greaterOrEqualCodecInner: Codec[GreaterOrEqual] =
    AttributeValueAssertion.codecInner.xmap(GreaterOrEqual, _.attribute)

  /**
    * Check that the given attribute is less or equal to the provided value.
    *
    * @param attribute  The attribute to be check.
    */
  case class LessOrEqual(
    attribute: AttributeValueAssertion
  ) extends Filter

  val lessOrEqualCodec: Codec[LessOrEqual] =
    AttributeValueAssertion.codecInner.xmap(LessOrEqual, _.attribute)

  /**
    * Check whether a given attribute exists.
    *
    * @param attribute  The attribute that should exists.
    */
  case class Present(
    attribute: AttributeDescription
  ) extends Filter

  // Codec without the BER wrapping
  val presentCodecInner: Codec[Present] =
    AttributeDescription.codecInner.xmap(Present, _.attribute)

  /**
    * Check using locally defined approximate matching that a given attributes match.
    * If there is no local approximate matching defined, this should be treated as [[EqualityMatch]]
    *
    * @param attribute  The attribute to be check against.
    */
  case class ApproxMatch(
    attribute: AttributeValueAssertion
  ) extends Filter

  // Codec without the BER wrapping
  val approxMatchCodecInner: Codec[ApproxMatch] =
    AttributeValueAssertion.codecInner.xmap(ApproxMatch, _.attribute)

  /** Extension to the filters, its operation is defined in [[MatchingRuleAssertion]] **/
  case class ExtensibleMatch(
    rule: MatchingRuleAssertion
  ) extends Filter

  // Codec without the BER wrapping
  val extensibleMatchCodecInner: Codec[ExtensibleMatch] =
    MatchingRuleAssertion.codecInner.xmap(ExtensibleMatch, _.rule)


  def codec: Codec[Filter] =
    ber.discriminated[Filter]
    .typecase(Identifier(ClassTag.Context, true, 0), ber.finiteLength(andCodecInner))
    .typecase(Identifier(ClassTag.Context, true, 1), ber.finiteLength(orCodecInner))
    .typecase(Identifier(ClassTag.Context, true, 2), ber.finiteLength(notCodecInner))
    .typecase(Identifier(ClassTag.Context, false, 3), ber.finiteLength(equalityCodecInner))
    .typecase(Identifier(ClassTag.Context, true, 4), ber.finiteLength(substringCodecInner))
    .typecase(Identifier(ClassTag.Context, false, 5), ber.finiteLength(greaterOrEqualCodecInner))
    .typecase(Identifier(ClassTag.Context, false, 6), ber.finiteLength(lessOrEqualCodec))
    .typecase(Identifier(ClassTag.Context, false, 7), ber.finiteLength(presentCodecInner))
    .typecase(Identifier(ClassTag.Context, false, 8), ber.finiteLength(approxMatchCodecInner))
    .typecase(Identifier(ClassTag.Context, false, 9), ber.finiteLength(extensibleMatchCodecInner))

}
