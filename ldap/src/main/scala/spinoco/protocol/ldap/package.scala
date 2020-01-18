package spinoco.protocol

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.BerClass

package object ldap {

  type LDAPString = String
  val ldapString: Codec[LDAPString] = ber.octetStringPrimitive.exmap(
    bv => Attempt.fromEither(bv.decodeUtf8.left.map(err => Err(err.toString)))
    , str => Attempt.fromEither(ByteVector.encodeUtf8(str).left.map(err => Err(err.toString)))
  )

  val ldapStringInner: Codec[LDAPString] =
    scodec.codecs.bytes.exmap(
      bv => Attempt.fromEither(bv.decodeUtf8.left.map(err => Err(err.toString)))
      , str => Attempt.fromEither(ByteVector.encodeUtf8(str).left.map(err => Err(err.toString)))
    )

  type MatchingRuleId = LDAPString
  val matchingRuleId: Codec[MatchingRuleId] = ldapString
  val matchingRuleIdInner: Codec[MatchingRuleId] = ldapStringInner

  type AttributeValue = ByteVector
  val attributeValue: Codec[AttributeValue] = ber.octetStringPrimitive

  type AssertionValue = ByteVector
  val assertionValue: Codec[AssertionValue] = ber.octetStringPrimitive
  val assertionValueInner: Codec[AssertionValue] = scodec.codecs.bytes

  //LDAP requires a special BOOLEAN encoding, it is defined here.
  private val booleanLDAP: Codec[Boolean] = new Codec[Boolean] {

    val trueVector: BitVector = BitVector.high(8)
    val falseVector: BitVector = BitVector.low(8)

    def decode(bits: BitVector): Attempt[DecodeResult[Boolean]] = {
      Attempt.fromEither(
        bits.consume(8){ bits =>
          if (bits == trueVector) Right(true)
          else Right(false)
        }
        .map{ case (remaining, value) => DecodeResult(value, remaining)}
        .left.map(err => Err("Could not decode LDAP boolean due to: " + err))
      )
    }

    def encode(value: Boolean): Attempt[BitVector] = {
      if (value) Attempt.successful(trueVector)
      else Attempt.successful(falseVector)
    }

    def sizeBound: SizeBound = SizeBound.exact(8)
  }

  val boolean: Codec[Boolean] = ber.codecSingle(BerClass.Universal, false, 1)(booleanLDAP)

}
