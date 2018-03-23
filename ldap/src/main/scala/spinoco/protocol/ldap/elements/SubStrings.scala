package spinoco.protocol.ldap.elements

import scodec.{Attempt, Codec, Err}
import scodec.bits.ByteVector
import spinoco.protocol.ldap.assertionValueInner
import scodec.codecs._
import spinoco.protocol.asn.ber
import spinoco.protocol.asn.ber.{BerClass, Identifier}
import spinoco.protocol.ldap.elements.SubStrings.{Anywhere, Final, Initial}

/**
  * A filter for a format of a given value.
  *
  * @param initial  The attribute value should start with this value.
  * @param any      These values should appear anywhere in the attribute value.
  * @param last     The attribute value should end with this value.
  */
case class SubStrings(
  initial: Option[Initial]
  , any: Vector[Anywhere]
  , last: Option[Final]
)

object SubStrings {

  case class Initial(value: ByteVector) extends SubStringAssertionType
  case class Anywhere(value: ByteVector) extends SubStringAssertionType
  case class Final(value: ByteVector) extends SubStringAssertionType

  sealed trait SubStringAssertionType

  val codecSubStringAssertion: Codec[SubStringAssertionType] =
    ber.discriminated[SubStringAssertionType]
    .typecase(Identifier(BerClass.Context, false, 0), ber.finiteLength(assertionValueInner.xmap[Initial](Initial, _.value)))
    .typecase(Identifier(BerClass.Context, false, 1), ber.finiteLength(assertionValueInner.xmap[Anywhere](Anywhere, _.value)))
    .typecase(Identifier(BerClass.Context, false, 2), ber.finiteLength(assertionValueInner.xmap[Final](Final, _.value)))


  val codec: Codec[SubStrings] =
    ber.sequence(vector(codecSubStringAssertion).narrow(
      assertions => {
        val initial = assertions.collect{ case init: Initial => init}
        val last = assertions.collect{ case fin: Final => fin}
        val any = assertions.collect{ case any: Anywhere => any }

        if (initial.size > 1 || last.size > 1) {
          Attempt.failure(Err(s"Substring filter's substrings have to contain at most only 1 initial and 1 final values, was initial: $initial and final: $last"))
        } else Attempt.successful(SubStrings(initial.headOption, any, last.headOption))
      }
      , subStrings => subStrings.initial.toVector ++ subStrings.any ++ subStrings.last
    ))

}