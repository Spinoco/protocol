package spinoco.protocol.email.header

import java.time.ZonedDateTime

import scodec.Codec
import spinoco.protocol.email.header.codec.DateTimeCodec

/**
  * RFC 5322 3.6.6:
  *
  * Resent fields SHOULD be added to any message that is reintroduced by
  * a user into the transport system.  A separate set of resent fields
  * SHOULD be added each time this is done.
  *
  * For instance, the "Resent-Date:" field corresponds to
  * the "Date:" field
  *
  */
case class ResentDate(date: ZonedDateTime) extends EmailHeaderField {
  def name: String = ResentDate.name
}


object ResentDate extends HeaderDescription[ResentDate]{

  val name: String = "Resent-Date"

  val codec: Codec[ResentDate] =
    DateTimeCodec.codec.xmap(ResentDate.apply, _.date)

  def fieldCodec: Codec[EmailHeaderField] = codec.upcast
}