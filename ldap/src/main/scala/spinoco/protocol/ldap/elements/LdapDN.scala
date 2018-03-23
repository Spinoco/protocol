package spinoco.protocol.ldap.elements

import scodec.bits.ByteVector
import scodec.{Attempt, Codec, Err}
import spinoco.protocol.ldap
import spinoco.protocol.ldap.elements.LdapDN.RelativeDistinguishedName

/**
  * Representation of distinguished name, defined in RFC 4514.
  *
  * @param names The vector of relative names. This vector can be empty, that denotes the root object.
  */
case class LdapDN(
  names: Vector[RelativeDistinguishedName]
)

object LdapDN {

  // Codec with BER wrapper around it
  val codec: Codec[LdapDN] = ldap.ldapString.exmap(decode, encode)

  // Codec without the BER wrapping
  val codecInner: Codec[LdapDN] = ldap.ldapStringInner.exmap(decode, encode)

  /** A set of values that distinguish an unique entry. Has to be nonempty. **/
  case class RelativeDistinguishedName(
    attributes: Set[AttributeTypeAndValue]
  )

  /**
    * An value with its value.
    *
    * This can bee seen as [[AttributeValueAssertion]] but it is string encoded.
    *
    * @param tpe    The type of the attribute.
    * @param value  The value of the attribute.
    */
  case class AttributeTypeAndValue(
    tpe: AttributeDescription
    , value: String
  )

  // The set of characters that we escape with a slash in front of them.
  val slashEscapable: Set[Char] = Set('\"', '+', ',', ';', '<', '=', '>', '\\')

  // The set of all characters that can appear in the encoded string and be escaped with a slash.
  val slashEscaped: Set[Char] = slashEscapable ++ Set(' ', '#')

  /**
    * Decodes a LDAPDN from an encoded string.
    *
    * This searches for:
    *   Non escaped '+', which denotes the end of an attribute inside current relativeDN.
    *    - take the current accumulator and try to parse out type and value of the attribute.
    *
    *   Non escaped ',', which denotes the end of the current relativeDN.
    *     - take the current accumulator and try to parse out type and value of the attribute.
    *
    * @param string The string encoded LDAPDN.
    */
  def decode(string: String): Attempt[LdapDN] = {
    def go(remaining: String, acc: String, parsedRelative: Vector[RelativeDistinguishedName], parsedAttributes: Set[AttributeTypeAndValue]): Attempt[LdapDN] = {
      remaining.headOption match {
        case None =>
          if (acc.isEmpty) {
            Attempt.successful(LdapDN(parsedRelative))
          } else {
            decodeTypeAndValue(acc) match {
              case Attempt.Successful(attr) => Attempt.successful(LdapDN(parsedRelative :+ RelativeDistinguishedName(parsedAttributes + attr)))
              case Attempt.Failure(err) => Attempt.Failure(err)
            }
          }

        case Some(',') =>
          decodeTypeAndValue(acc) match {
            case Attempt.Successful(attr) => go(remaining.drop(1), "", parsedRelative :+ RelativeDistinguishedName(parsedAttributes + attr), Set.empty)
            case Attempt.Failure(err) => Attempt.Failure(err)
          }

        case Some('+') =>
          decodeTypeAndValue(acc) match {
            case Attempt.Successful(attr) => go(remaining.drop(1), "", parsedRelative, parsedAttributes + attr)
            case Attempt.Failure(err) => Attempt.Failure(err)
          }

        case Some(c @ '\\') =>
          remaining.drop(1).headOption match {
            case None =>
              Attempt.failure(Err("Encountered escape character but no value to be escaped"))

            case Some(ce) if slashEscaped.contains(ce) =>
              go(remaining.drop(2), acc + c + ce, parsedRelative, parsedAttributes)

            case Some(_) =>
              go(remaining.drop(3), acc + remaining.take(3), parsedRelative, parsedAttributes)
          }

        case Some(c) => go(remaining.drop(1), acc + c, parsedRelative, parsedAttributes)
      }
    }

    go(string, "", Vector.empty, Set.empty)
  }

  /**
    * Encodes the provided LDAPDN into a string. This will only fail in case any
    * of the relativeDNs are empty.
    *
    * @param dn The LDAPDN to be encoded.
    */
  def encode(dn: LdapDN): Attempt[String] = {
    dn.names.foldLeft(Attempt.successful(Vector.empty[String])){ case (acc, relative) =>
      acc.flatMap{ curr =>
        encodeRelative(relative).map(curr :+ _ )
      }
    }.map(_.mkString(","))
  }

  /**
    * Encodes the provided relativeDN into its string representation. This only fails
    * if the relativeDN has no attribute in it.
    *
    * @param relative The relativeDN that is to be encoded into a string.
    */
  def encodeRelative(relative: RelativeDistinguishedName): Attempt[String] = {
    if (relative.attributes.isEmpty) Attempt.failure(Err("Relative distinguished name has to have at least one value"))
    else Attempt.successful {
      relative.attributes.map(encodeTypeAndValue).mkString("+")
    }
  }

  /**
    * Unescapes the value of [[AttributeTypeAndValue]], which follows the RFC 4514 section 2.4.
    *
    * @param string The value that was escaped according to the RFC
    */
  def unescapeValue(string: String): Attempt[String] = {
    def go(remains: String, acc: String): Attempt[String] = {
      remains.headOption match {
        case None => Attempt.successful(acc)
        case Some(c) =>
          val tail = remains.drop(1)
          if (c == '\\') {
            tail.headOption match {
              case None => Attempt.failure(Err("No characters to unescape, while encountered escape character"))
              case Some(next) if slashEscaped.contains(next) => go(tail.drop(1), acc + next)
              case _ =>
                //Decoding UTF-8 since that is the LDAP RFC says
                Attempt.fromOption(ByteVector.fromHex(tail.take(2)), Err(s"The escaped values: ${tail.take(2)} is not a valid hex")).flatMap{ bytes =>
                  Attempt.fromEither(bytes.decodeUtf8.left.map(err => Err(err.toString)))
                } match {
                  case Attempt.Successful(decoded) => go(tail.drop(2), acc + decoded)
                  case Attempt.Failure(err) => Attempt.failure(err)
                }
            }
          } else go(tail, acc + c)
      }
    }

    go(string, "")
  }

  /**
    * Escapes the value of [[AttributeTypeAndValue]], which follows the RFC 4514 section 2.4.
    *
    * @param value The value that is to be escaped.
    */
  def escapeValue(value: String): String = {
    def go(remains: String, acc: String): String = {
      remains.headOption match {
        case None => acc
        case Some(c) if slashEscapable.contains(c) => go(remains.drop(1), acc + s"\\$c")
        case Some('\u0000') => go(remains.drop(1), acc + "\\00")
        case Some(' ') if remains.length == 1 => acc ++ "\\20"
        case Some(c) => go(remains.drop(1), acc + c)
      }
    }

    value.headOption match {
      case None => ""
      case Some('#') => go(value.drop(1), "\\23")
      case Some(' ') => go(value.drop(1), "\\00")
      case Some(_) => go(value, "")
    }
  }

  /**
    * Decodes the type and and its value from its string represnetation.
    *
    * NOTE that if the type is that of [[AttributeDescription.DottedDecimal]] form
    * then we do not unescape its values. The value of such attribute is that of the hex of
    * BER encoded value.
    *
    * @param string The string that has an attribute and value encoded in it.
    */
  def decodeTypeAndValue(string: String): Attempt[AttributeTypeAndValue] = {
    // Splitting by first =, as there has to be equal after the type
    // we are safe to do this even when there are some other = escaped in the \= form
    val splitIdx = string.indexOf("=")
    if (splitIdx > 0) {
      val (tpe, value) = string.splitAt(splitIdx)
      val valueWithoutEquals = value.drop(1)
      for {
        tpe <- AttributeDescription.decodeTpe(tpe).mapErr(_.pushContext("TypeAndValue"))
        decodedValue <- tpe match {
          case _: AttributeDescription.DottedDecimal =>
            if (valueWithoutEquals.startsWith("#")) {
              Attempt.successful(valueWithoutEquals.drop(1)) //Dropping the #
            } else Attempt.failure(Err("TypeAndValue defined with dotted decimal's value does start with #."))

          case _: AttributeDescription.TextDescriptor | _: AttributeDescription.Recognised =>
            unescapeValue(valueWithoutEquals)
        }
      } yield AttributeTypeAndValue(tpe, decodedValue)

    } else Attempt.failure(Err("Incorrect tpe and value, missing ="))
  }

  /**
    * Encodes the provided attribute into its string representation.
    *
    * If the type of the attribute is that of [[AttributeDescription.DottedDecimal]]
    * then the value is expected to be in the hex of the BER encoding. As such
    * there is no escaping done for such value.
    *
    * @param tpeAndValue  The attribute to be encoded.
    */
  def encodeTypeAndValue(tpeAndValue: AttributeTypeAndValue): String = {
    AttributeDescription.encodeTpe(tpeAndValue.tpe) + "=" + {
      tpeAndValue.tpe match {
        case _: AttributeDescription.DottedDecimal => "#" + tpeAndValue.value
        case _ => escapeValue(tpeAndValue.value)
      }
    }
  }
}
