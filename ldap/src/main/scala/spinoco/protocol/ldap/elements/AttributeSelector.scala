package spinoco.protocol.ldap.elements

import scodec.{Attempt, Codec}
import spinoco.protocol.asn.ber
import spinoco.protocol.ldap

/** Selector for LDAP object's attributes. */
sealed trait AttributeSelector

object AttributeSelector {

  /** Select all attributes of the matching objects. **/
  case object AllAttributes extends AttributeSelector

  /**
    * Select no attributes of the matching object. In case this selector
    * is mixed with other selectors that do select some values, this selector
    * has to be ignored.
    */
  case object NoAttributes extends AttributeSelector

  /**
    * A given attribute is selected.
    *
    * @param desc The description of the attribute that is to be selected.
    */
  case class Description(desc: AttributeDescription) extends AttributeSelector


  val codec: Codec[AttributeSelector] =
    ldap.ldapString.narrow(decodeSelector, encodeSelector)


  val selectionCodec: Codec[Vector[AttributeSelector]] =
    ber.sequence(scodec.codecs.vector(codec))

  /**
    * Decodes the given attribute selector from the supplied string.
    *
    * @param str  The string that contains the selector.
    */
  def decodeSelector(str: String): Attempt[AttributeSelector] = {
    if (str == "*") Attempt.successful(AllAttributes)
    else if (str == "1.1") Attempt.successful(NoAttributes)
    else AttributeDescription.decodeTpe(str).map(Description)
  }

  /**
    * Encodes a given selector into its string representation.
    *
    * @param selector The selector that is to be encoded.
    */
  def encodeSelector(selector: AttributeSelector): String = {
    selector match {
      case AllAttributes => "*"
      case NoAttributes => "1.1"
      case Description(desc) => AttributeDescription.encodeTpe(desc)
    }
  }
}