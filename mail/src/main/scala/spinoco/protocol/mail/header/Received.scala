package spinoco.protocol.mail.header

import java.time.ZonedDateTime

import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import spinoco.protocol.mail.header.codec.DateTimeCodec

/**
  * Created by pach on 17/10/17.
  */
case class Received(token: String, at: ZonedDateTime) extends DefaultEmailHeaderField


object Received extends DefaultHeaderDescription[Received] {

  def nonRFCCodec(datePosition: Int): Codec[Received] = {
    scodec.codecs.utf8.exmap(
      s => {
        val parts = s.trim.split(" ")
        val date = parts.takeRight(datePosition).mkString(" ")
        val token = parts.dropRight(datePosition).mkString(" ")

        DateTimeCodec.parseDate(date).map { zdt =>
          Received(token, zdt)
        }
      }
      , rcv => DateTimeCodec.formatDate(rcv.at) map { s => s"${rcv.token};\r\n $s" }
    )
  }

  val RFCCodec: Codec[Received] = {
    scodec.codecs.utf8.exmap(
      s => {
        val part = s.lastIndexOf(';')
        if (part < 0) Attempt.failure(Err(s"""Failed to parse header,  *received-token ";" date-time expected, got: $s"""))
        else {
          val (rem, dt) = s.splitAt(part)
          DateTimeCodec.parseDate(dt.tail.trim) map { zdt =>
            Received(rem, zdt)
          }
        }
      }
      , rcv => DateTimeCodec.formatDate(rcv.at) map { s => s"${rcv.token};\r\n $s" }
    )
  }

  val codec: Codec[Received] = choice(RFCCodec, nonRFCCodec(4), nonRFCCodec(7))

}
