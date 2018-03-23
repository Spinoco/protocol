package spinoco.protocol.ldap.elements

import scodec.{Attempt, Codec, Err}
import spinoco.protocol.ldap

import scala.util.Try

/** The identification of an attribute. */
sealed trait AttributeDescription

object AttributeDescription {

  val codec: Codec[AttributeDescription] = ldap.ldapString.narrow(decodeTpe, encodeTpe)

  // Codec without the BER wrapping
  val codecInner: Codec[AttributeDescription] = ldap.ldapStringInner.narrow(decodeTpe, encodeTpe)

  /**
    * The attribute is identified by a descriptor. That is a text representation of the attribute.
    * And is not one of the compulsory recognised attributes.
    *
    * @param value The descriptor for the attribute.
    */
  case class TextDescriptor(value: String) extends AttributeDescription

  /**
    * The attribute is identified by its OID description.
    * It is in a format of "0.1.2.x...."
    *
    * @param values In order values of the OID.
    */
  case class DottedDecimal(values: Vector[Int]) extends AttributeDescription

  /**
    * The attribute is one of the compulsory recognised attributes.
    *
    * @param tpe  The attribute that was recognised.
    */
  case class Recognised(tpe: AttributeType.Value) extends AttributeDescription

  /**
    * Decodes the Attribute description from a string.
    *
    * This will try to match the attribute against recognised attributes first.
    *
    * @param str  The string that is representing the type.
    */
  def decodeTpe(str: String): Attempt[AttributeDescription] = {
    if (str.contains(".")) {
      Attempt.fromOption[AttributeDescription](AttributeType.forOID(str).map(Recognised), Err("Could not match OID"))
      .recoverWith[AttributeDescription]{ case _ =>
        decodeDottedDecimal(str)
      }

    } else{
      Attempt.successful{
        Try(AttributeType.withName(str.toLowerCase())).toOption.map(Recognised).getOrElse(TextDescriptor(str))
      }
    }
  }

  /**
    * Decodes an attribute in form of a dotted decimal.
    *
    * This will fail if there are some other values than integers and dots.
    *
    * @param str The string that should contain the dotted decimal format.
    */
  def decodeDottedDecimal(str: String): Attempt[DottedDecimal] = {
    val split = str.split("\\.")
    Attempt.fromOption[DottedDecimal](
      Try(split.map(_.toInt)).map(v => DottedDecimal(v.toVector)).toOption
      , Err(s"Could not parse out decimals out of dotted format, gotten: $str")
    )
  }

  /**
    * Encodes the description into a string that represents the attribute.
    *
    * @param tpe  The attribute description that is to be encoded.
    */
  def encodeTpe(tpe: AttributeDescription): String = {
    tpe match {
      case TextDescriptor(value) => value
      case DottedDecimal(values) => values.mkString(".")
      case Recognised(value) => value.toString
    }
  }

  //Compulsory types to be recognised as per RFC 4514 section 3
  object AttributeType extends Enumeration {
    val commonName = Value("cn")
    val localityName = Value("l")
    val stateOrProvinceName = Value("st")
    val organizationName = Value("o")
    val organizationalUnitName = Value("ou")
    val countryName = Value("c")
    val streetAddress = Value("street")
    val domainComponent = Value("dc")
    val userId = Value("uid")

    def forOID(dottedDecimal: String): Option[AttributeType.Value] = {
      dottedDecimal match {
        case "2.5.4.3" => Some(AttributeType.commonName)
        case "2.5.4.7" => Some(AttributeType.localityName)
        case "2.5.4.8" => Some(AttributeType.stateOrProvinceName)
        case "2.5.4.10" => Some(AttributeType.organizationName)
        case "2.5.4.11" => Some(AttributeType.organizationalUnitName)
        case "2.5.4.6" => Some(AttributeType.countryName)
        case "2.5.4.9" => Some(AttributeType.streetAddress)
        case "0.9.2342.19200300.100.1.25" => Some(AttributeType.domainComponent)
        case "0.9.2342.19200300.100.1.1" => Some(AttributeType.userId)
        case _ => None
      }
    }

  }

}
