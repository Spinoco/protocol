package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.header.codec.EmailAddressCodec

/**
  * RFC 5322 3.6.6:
  *
  * Resent fields SHOULD be added to any message that is reintroduced by
  * a user into the transport system.  A separate set of resent fields
  * SHOULD be added each time this is done.
  *
  * For instance, the "Resent-Sender:" field corresponds to
  * the "Sender:" field
  *
  */
case class `Resent-Sender`(sender: EmailAddress) extends DefaultEmailHeaderField



object `Resent-Sender` extends DefaultHeaderDescription[`Resent-Sender`] {

  val codec: Codec[`Resent-Sender`] = {
    EmailAddressCodec.codec.xmap(
      `Resent-Sender`.apply, _.sender
    )
  }


}