package spinoco.protocol.email.header

import scodec.Codec
import spinoco.protocol.email.EmailAddress
import spinoco.protocol.email.header.codec.EmailAddressCodec

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
case class ResentSender(sender: EmailAddress) extends EmailHeaderField {
  def name: String = ResentSender.name
}



object ResentSender extends HeaderDescription[ResentSender] {

  val name: String = "Sender"

  val codec: Codec[ResentSender] = {
    EmailAddressCodec.codec.xmap(
      ResentSender.apply, _.sender
    )
  }

  def fieldCodec: Codec[EmailHeaderField] = codec.upcast

}