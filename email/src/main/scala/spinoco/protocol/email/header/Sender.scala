package spinoco.protocol.email.header

import scodec.Codec
import spinoco.protocol.email.EmailAddress
import spinoco.protocol.email.header.codec.EmailAddressCodec

/**
  * RFC 5322 3.6.2:
  *
  * if a secretary were to send a message for
  * another person, the mailbox of the secretary would appear in the
  * "Sender:" field and the mailbox of the actual author would appear in
  * the "From:" field.
  */
case class Sender(sender: EmailAddress) extends EmailHeaderField {
  def name: String = Sender.name
}


object Sender extends HeaderDescription[Sender] {

  val name: String = "Sender"

  val codec: Codec[Sender] = {
    EmailAddressCodec.codec.xmap(
      Sender.apply, _.sender
    )
  }


  def fieldCodec: Codec[EmailHeaderField] = codec.upcast
}
