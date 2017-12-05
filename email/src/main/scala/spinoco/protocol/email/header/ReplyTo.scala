package spinoco.protocol.email.header

import scodec.Codec
import spinoco.protocol.email.EmailAddress
import spinoco.protocol.email.header.codec.{EmailAddressCodec, commaSeparated}

/**
  * RFC 5322 3.6.2:
  *
  * In either
  * case, an optional reply-to field MAY also be included, which contains
  * the field name "Reply-To" and a comma-separated list of one or more
  * addresses.
  */
case class ReplyTo(email: EmailAddress, others: List[EmailAddress]) extends EmailHeaderField {
  def name: String = ReplyTo.name
}

object ReplyTo extends HeaderDescription[ReplyTo] {
  val name: String = "Reply-To"

  val codec: Codec[ReplyTo] = {
    commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
      ReplyTo.apply _ tupled, rplyTo => (rplyTo.email, rplyTo.others)
    )
  }


  def fieldCodec: Codec[EmailHeaderField] = codec.upcast

}
