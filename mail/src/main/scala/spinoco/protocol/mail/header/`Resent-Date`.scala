package spinoco.protocol.mail.header

import java.time.ZonedDateTime

import scodec.Codec
import spinoco.protocol.mail.header.codec.DateTimeCodec

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
case class `Resent-Date`(date: ZonedDateTime) extends DefaultEmailHeaderField


object `Resent-Date` extends DefaultHeaderDescription[`Resent-Date`] {
  val codec: Codec[`Resent-Date`] =
    DateTimeCodec.codec.xmap(`Resent-Date`.apply, _.date)

}