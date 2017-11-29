package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.header.codec.{EmailAddressCodec, commaSeparated}


/**
  * RFC 5322 3.6.3.
  *
  * The destination fields of a message consist of three possible fields,
  * each of the same form: the field name, which is either "To", "Cc", or
  * "Bcc", followed by a comma-separated list of one or more addresses
  *
  * @param tpe        Type of the destination (To, Cc, Bcc)
  * @param email      Destination address
  * @param others     Other Email addresses
  */
case class Destination(
  tpe: DestinationType.Value
  , email: EmailAddress
  , others: List[EmailAddress]
) extends EmailHeaderField {
  lazy val name: String = tpe match {
    case DestinationType.To => Destination.To.name
    case DestinationType.Cc => Destination.Cc.name
    case DestinationType.Bcc => Destination.Bcc.name
  }
}


object Destination {

  object To extends HeaderDescription[Destination] {

    val name: String = "To"

    val codec: Codec[Destination] = {
      commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
        { case (email, others) => Destination(DestinationType.To, email, others) }, dest => (dest.email, dest.others)
      )
    }

    def emailHeaderField: Codec[EmailHeaderField] = codec.upcast

  }

  object Cc extends HeaderDescription[Destination] {
    val name: String = "Cc"

    val codec: Codec[Destination] = {
      commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
        { case (email, others) => Destination(DestinationType.Cc, email, others) }, dest => (dest.email, dest.others)
      )
    }

    def emailHeaderField: Codec[EmailHeaderField] = codec.upcast

  }

  object Bcc extends HeaderDescription[Destination] {
    val name: String = "Bcc"

    val codec: Codec[Destination] = {
      commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
        { case (email, others) => Destination(DestinationType.Bcc, email, others) }, dest => (dest.email, dest.others)
      )
    }

    def emailHeaderField: Codec[EmailHeaderField] = codec.upcast

  }

}


/** type of message destination **/
object DestinationType extends Enumeration {
  val To, Cc, Bcc = Value

}


