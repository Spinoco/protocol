package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.header.codec.{EmailAddressCodec, commaSeparated}

/**
  * RFC 5322 3.6.2.
  *
  *  The "From:" field specifies the author(s) of the message,
  *  that is, the mailbox(es) of the person(s) or system(s) responsible
  *  for the writing of the message.
  *
  *
  *  @param email     Email of the person
  *  @param others    List of other autors of this email.
  *
  */
case class From(
  email: EmailAddress
  , others: List[EmailAddress]
) extends DefaultEmailHeaderField


object From extends DefaultHeaderDescription[From] {
  val codec: Codec[From] = {
    commaSeparated(EmailAddressCodec.codec, fold = true).xmap(
      From.apply _ tupled, from => (from.email, from.others)
    )
  }
}