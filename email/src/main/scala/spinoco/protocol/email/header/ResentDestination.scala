package spinoco.protocol.email.header

import scodec.Codec
import spinoco.protocol.email.EmailAddress
import spinoco.protocol.email.header.codec.{EmailAddressCodec, commaSeparated}

/**
  * RFC 5322 3.6.6:
  *
  * Resent fields SHOULD be added to any message that is reintroduced by
  * a user into the transport system.  A separate set of resent fields
  * SHOULD be added each time this is done.
  *
  * For instance, the "Resent-To:" field corresponds to
  * the "To:" field
  *
  * @param tpe        Type of the destination (To, Cc, Bcc)
  * @param email      Destination address
  * @param others     Other Email addresses
  */
case class ResentDestination(
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


object ResentDestination {

  object To extends HeaderDescription[ResentDestination] {

    val name: String = "Resent-To"

    val codec: Codec[ResentDestination] = {
      commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
        { case (email, others) => ResentDestination(DestinationType.To, email, others) }, dest => (dest.email, dest.others)
      )
    }

    def fieldCodec: Codec[EmailHeaderField] = codec.upcast

  }

  object Cc extends HeaderDescription[ResentDestination] {
    val name: String = "Resent-Cc"

    val codec: Codec[ResentDestination] = {
      commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
        { case (email, others) => ResentDestination(DestinationType.Cc, email, others) }, dest => (dest.email, dest.others)
      )
    }

    def fieldCodec: Codec[EmailHeaderField] = codec.upcast

  }

  object Bcc extends HeaderDescription[ResentDestination] {
    val name: String = "Resent-Bcc"

    val codec: Codec[ResentDestination] = {
      commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
        { case (email, others) => ResentDestination(DestinationType.Bcc, email, others) }, dest => (dest.email, dest.others)
      )
    }

    def fieldCodec: Codec[EmailHeaderField] = codec.upcast

  }

}



