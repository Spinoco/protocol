package spinoco.protocol.mime

import java.nio.charset.{Charset, StandardCharsets}

import scodec.{Attempt, Codec}
import spinoco.protocol.common.util._


sealed case class MIMECharset(value: String, aliases: Seq[String])


object MIMECharset {

  val `US-ASCII`     = MIMECharset("US-ASCII", Seq("iso-ir-6", "ANSI_X3.4-1968", "ANSI_X3.4-1986", "ISO_646.irv:1991", "ASCII", "ISO646-US", "us", "IBM367", "cp367", "csASCII"))
  val `ISO-8859-1`   = MIMECharset("ISO-8859-1", Seq("iso-ir-100", "ISO_8859-1", "latin1", "l1", "IBM819", "CP819", "csISOLatin1"))
  val `UTF-8`        = MIMECharset("UTF-8", Seq("UTF8"))
  val `UTF-16`       = MIMECharset("UTF-16", Seq("UTF16"))
  val `UTF-16BE`     = MIMECharset("UTF-16BE", Nil)
  val `UTF-16LE`     = MIMECharset("UTF-16LE", Nil)


  val allDefault : Map[String, MIMECharset] = Seq(
    `US-ASCII`
      , `ISO-8859-1`
      , `UTF-8`
      , `UTF-16`
      , `UTF-16BE`
      , `UTF-16LE`
  ).map { chs => chs.value.toLowerCase -> chs }.toMap

  val codec: Codec[MIMECharset] = {
    import scodec.codecs._

    string(StandardCharsets.US_ASCII).xmap(
      s => allDefault.getOrElse(s.toLowerCase, MIMECharset(s, Nil))
      , _.value.toLowerCase
    )
  }

  def forJavaCharset(charset: Charset): MIMECharset =
    MIMECharset(charset.name().toUpperCase, Nil)

  def asJavaCharset(charset: MIMECharset): Attempt[Charset] =
    attempt(Charset.forName(charset.value))


}