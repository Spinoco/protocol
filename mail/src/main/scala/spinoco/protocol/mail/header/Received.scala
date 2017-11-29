package spinoco.protocol.mail.header

import java.time.ZonedDateTime

import scodec.{Attempt, Codec, Err}
import spinoco.protocol.mail.header.codec.DateTimeCodec

/**
  * Created by pach on 17/10/17.
  */
case class Received(token: String, at: ZonedDateTime) extends DefaultEmailHeaderField


object Received  extends DefaultHeaderDescription[Received] {

  val codec: Codec[Received] = {
    scodec.codecs.utf8.exmap(
      s => {
        val rev = s.reverse
        val part = rev.indexOf(';')
        if (part < 0) Attempt.failure(Err(s"""Failed to parse header,  *received-token ";" date-time expectd, got: $s"""))
        else {
          val (revDt, revRem) = rev.splitAt(part)
          DateTimeCodec.parseDate(revDt.trim.reverse) map { zdt =>
            Received(revRem.reverse.init, zdt)
          }
        }
      }
      , rcv => DateTimeCodec.formatDate(rcv.at) map { s => s"${rcv.token};\r\n $s" }
    )
  }


}
