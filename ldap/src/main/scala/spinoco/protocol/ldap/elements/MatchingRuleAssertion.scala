package spinoco.protocol.ldap.elements

import scodec.Codec
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.ClassTag
import spinoco.protocol.ldap._
import spinoco.protocol.{common, ldap}

/**
  * Extension for filter matching. Follows these rules:
  *
  *   - If the matchingRule field is absent, the type field MUST be
  *     present, and an equality match is performed for that type.
  *
  *   - If the type field is absent and the matchingRule is present, the
  *     matchValue is compared against all attributes in an entry that
  *     support that matchingRule.
  *
  *   - If the type field is present and the matchingRule is present, the
  *     matchValue is compared against the specified attribute type and its
  *     subtypes.
  *
  * @param matchingRule The rule which is to be used for matching, if not present, then this is an equality match.
  * @param tpe          The entry against which the matching is done. If not specified, the match
  *                     is done against all entries.
  * @param matchValue   The desired value for the matching.
  * @param dnAttributes If set to true then the matching should be done against the DN attributes of the
  *                     entry as well. And the matching is true only if there is at least one value in the DN
  *                     that is true for this match.
  */
case class MatchingRuleAssertion(
  matchingRule: Option[MatchingRuleId]
  , tpe: Option[AttributeDescription]
  , matchValue: AssertionValue
  , dnAttributes: Boolean
)

object MatchingRuleAssertion {

  // Codec without the BER wrapping
  val codecInner: Codec[MatchingRuleAssertion] =
    (common.codec.maybe(ber.codecSingle(ClassTag.Context, false, 1)(matchingRuleIdInner)) ::
    common.codec.maybe(ber.codecSingle(ClassTag.Context, false, 2)(AttributeDescription.codecInner)) ::
    ber.codecSingle(ClassTag.Context, false, 3)(assertionValueInner) ::
    common.codec.default(ber.codecSingle(ClassTag.Context, false, 4)(ldap.boolean), false)).as[MatchingRuleAssertion]

}
