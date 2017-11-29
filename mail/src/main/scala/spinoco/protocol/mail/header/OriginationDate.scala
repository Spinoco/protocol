package spinoco.protocol.mail.header

import java.time.ZonedDateTime

import scodec.Codec
import spinoco.protocol.mail.header.codec.DateTimeCodec

/**
  * RFC 5322 3.6.1
  *
  * The origination date specifies the date and time at which the creator
  * of the message indicated that the message was complete and ready to
  * enter the mail delivery system.
  */
case class OriginationDate(date: ZonedDateTime) extends EmailHeaderField {
  def name: String = OriginationDate.name
}


object OriginationDate extends HeaderDescription[OriginationDate]{

  val name: String = "Date"

  val codec: Codec[OriginationDate] =
    DateTimeCodec.codec.xmap(OriginationDate.apply, _.date)

  def emailHeaderField: Codec[EmailHeaderField] = codec.upcast

}
