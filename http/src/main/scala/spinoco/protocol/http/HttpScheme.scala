package spinoco.protocol.http

import scodec.{Codec, Err}
import codec.helper._

/**
  * A generic protocol for Uri.
  *
  * @param tpe  The type of the protocol.
  */
case class Scheme private[http](tpe: String)

object Scheme {

  def parseScheme(toParse: String): Option[Scheme] = {
    toParse.headOption.flatMap { firstChar =>
      if (!firstChar.isLetter) None
      else if (toParse.tail.forall(c => c.isLetterOrDigit || c == '+' || c == '-' || c == '.')) {
        Some(new Scheme(toParse.toLowerCase))
      } else None
    }
  }

  val codec: Codec[Scheme] = {
    import scodec.Attempt._
    trimmedAsciiToken.exmap(
      s => fromOption(parseScheme(s), Err(s"The specified scheme is not valid according to RFC 3986: $s"))
      , s => successful(s.tpe)
    )
  }

}

object HttpScheme {

  val HTTP = Scheme("http")

  val HTTPS = Scheme("https")

  val WS = Scheme("ws")

  val WSS = Scheme("wss")

}
