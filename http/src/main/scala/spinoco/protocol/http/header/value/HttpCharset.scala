package spinoco.protocol.http.header.value

import java.nio.charset.StandardCharsets

import scodec.Codec

/**
  * Created by pach on 12/01/17.
  */
sealed case class HttpCharset(value: String, aliases: Seq[String])


object HttpCharset {

  val `US-ASCII`     = HttpCharset("US-ASCII", Seq("iso-ir-6", "ANSI_X3.4-1968", "ANSI_X3.4-1986", "ISO_646.irv:1991", "ASCII", "ISO646-US", "us", "IBM367", "cp367", "csASCII"))
  val `ISO-8859-1`   = HttpCharset("ISO-8859-1", Seq("iso-ir-100", "ISO_8859-1", "latin1", "l1", "IBM819", "CP819", "csISOLatin1"))
  val `UTF-8`        = HttpCharset("UTF-8", Seq("UTF8"))
  val `UTF-16`       = HttpCharset("UTF-16", Seq("UTF16"))
  val `UTF-16BE`     = HttpCharset("UTF-16BE", Nil)
  val `UTF-16LE`     = HttpCharset("UTF-16LE", Nil)


  val allDefault : Map[String, HttpCharset] = Seq(
    `US-ASCII`
      , `ISO-8859-1`
      , `UTF-8`
      , `UTF-16`
      , `UTF-16BE`
      , `UTF-16LE`
  ).map { chs => chs.value.toLowerCase -> chs }.toMap

  val codec: Codec[HttpCharset] = {
    import scodec.codecs._

    string(StandardCharsets.US_ASCII).xmap(
      s => allDefault.getOrElse(s.toLowerCase, HttpCharset(s, Nil))
      , _.value.toLowerCase
    )
  }

}