package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.header.codec.EmailAddressCodec

/**
  * RFC 5322 3.6.2:
  *
  * if a secretary were to send a message for
  * another person, the mailbox of the secretary would appear in the
  * "Sender:" field and the mailbox of the actual author would appear in
  * the "From:" field.
  */
case class Sender(sender: EmailAddress) extends DefaultEmailHeaderField


object Sender extends DefaultHeaderDescription[Sender] {

  val codec: Codec[Sender] = {
    EmailAddressCodec.codec.xmap(
      Sender.apply, _.sender
    )
  }

}
