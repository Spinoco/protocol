package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.header.codec.{EmailAddressCodec, commaSeparated}

/**
  * RFC 5322 3.6.2:
  *
  * In either
  * case, an optional reply-to field MAY also be included, which contains
  * the field name "Reply-To" and a comma-separated list of one or more
  * addresses.
  */
case class `Reply-To`(email: EmailAddress, others: List[EmailAddress]) extends DefaultEmailHeaderField

object `Reply-To` extends DefaultHeaderDescription[`Reply-To`] {
  val codec: Codec[`Reply-To`] = {
    commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
      `Reply-To`.apply _ tupled, rplyTo => (rplyTo.email, rplyTo.others)
    )
  }


}
