package spinoco.protocol.mail.header.codec

import java.nio.charset.StandardCharsets
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import scodec.{Attempt, Codec}
import scodec.codecs._
import spinoco.protocol.common.util._


object DateTimeCodec {

 val EmailDateFormatter : DateTimeFormatter =
   DateTimeFormatter.ofPattern("EEE, d MMM yyyy HH:mm:ss Z")

 val EmailDateFormatterNoSec : DateTimeFormatter =
   DateTimeFormatter.ofPattern("EEE, d MMM yyyy HH:mm Z")


  val EmailNoDayNameDateFormatter : DateTimeFormatter =
    DateTimeFormatter.ofPattern("d MMM yyyy HH:mm:ss Z")

  val EmailNoDayNameDateFormatterNoSec : DateTimeFormatter =
    DateTimeFormatter.ofPattern("d MMM yyyy HH:mm Z")


  val codec: Codec[ZonedDateTime] = {
    string(StandardCharsets.US_ASCII).exmap(
      s => parseDate(s.replaceAll("\\s+", " ")) // need to remove whitestapce other than single space
      , formatDate
    )
  }

  /** formats date according to RFC 5322 **/
  def formatDate(zdt: ZonedDateTime): Attempt[String] = {
    attempt(EmailDateFormatter.format(zdt))
  }

  /** parses date according to RFC 5322 **/
  def parseDate(s: String): Attempt[ZonedDateTime] = {
    // strip any named zone parameters  that occur after opening (
    val woZoneName = {
      val start = s.indexOf('(')
      if (start < 0) s.trim
      else s.take(start).trim
    }
    attempt(ZonedDateTime.parse(woZoneName, EmailDateFormatter)) orElse
    attempt(ZonedDateTime.parse(woZoneName, EmailDateFormatterNoSec)) orElse
    attempt(ZonedDateTime.parse(woZoneName, EmailNoDayNameDateFormatter)) orElse
    attempt(ZonedDateTime.parse(woZoneName, EmailNoDayNameDateFormatterNoSec))
  }




}
