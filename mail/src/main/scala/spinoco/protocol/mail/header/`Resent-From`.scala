package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.header.codec.{EmailAddressCodec, commaSeparated}

/**
  * RFC 5322 3.6.6:
  *
  * Resent fields SHOULD be added to any message that is reintroduced by
  * a user into the transport system.  A separate set of resent fields
  * SHOULD be added each time this is done.
  *
  * For instance, the "Resent-From:" field corresponds to
  * the "From:" field
  *
  */
case class `Resent-From`(
  email: EmailAddress
  , others: List[EmailAddress]
) extends DefaultEmailHeaderField

object `Resent-From` extends DefaultHeaderDescription[`Resent-From`] {
  val codec: Codec[`Resent-From`] = {
    commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
      `Resent-From`.apply _ tupled, from => (from.email, from.others)
    )
  }

}


