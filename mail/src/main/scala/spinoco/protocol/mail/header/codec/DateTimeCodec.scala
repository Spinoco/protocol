package spinoco.protocol.mail.header.codec

import java.nio.charset.StandardCharsets
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import scodec.codecs._
import scodec.{Attempt, Codec}
import spinoco.protocol.common.util._

object DateTimeCodec {

  val EmailWriteDateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("EEE, d MMM yyyy HH:mm:ss Z")

  val EmailReadDateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("[EEE, ]d MMM yyyy HH:mm[:ss[.n]] [Z][z]")

  val nonRFCFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-d HH:mm:ss.n Z z")

  val codec: Codec[ZonedDateTime] = {
    string(StandardCharsets.US_ASCII).exmap(
      s => parseDate(s.replaceAll("\\s+", " ")) // need to remove whitestapce other than single space
      , formatDate
    )
  }

  /** formats date according to RFC 5322 **/
  def formatDate(zdt: ZonedDateTime): Attempt[String] = {
    attempt(EmailWriteDateFormatter.format(zdt))
  }


  /** parses date according to RFC 5322 **/
  def parseDate(s: String): Attempt[ZonedDateTime] = {
    // strip any named zone parameters  that occur after opening (
    val woZoneName = {
      val start = s.indexOf('(')
      if (start < 0) s.trim
      else s.take(start).trim
    }

    attempt(ZonedDateTime.parse(woZoneName, EmailReadDateFormatter))
    .orElse(attempt(ZonedDateTime.parse(woZoneName, nonRFCFormatter)))
    .recoverWith { case _ if woZoneName.lift(3).contains(',') =>
      // If the exact date is wrong, try to strip it.
      // Dropping the "EEE, " part from EmailReadDateFormatter.
      attempt(ZonedDateTime.parse(woZoneName.drop(5), EmailReadDateFormatter))
    }

  }

}
