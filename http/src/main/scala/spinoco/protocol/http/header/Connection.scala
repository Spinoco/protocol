package spinoco.protocol.http.header

import scodec.{Attempt, Err}
import scodec.codecs._
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Connection
  */
sealed case class Connection(value: List[String]) extends DefaultHeader



object Connection { val codec =
  HeaderCodecDefinition[Connection](
    choice(
      asciiConstant("close").exmap[List[String]](_ => Attempt.successful(Nil), l => if (l.isEmpty) Attempt.successful(()) else Attempt.failure(Err("nonEmpty")))
      , commaDelimitedMin(trimmedAsciiString, 1)
    ).xmap(Connection.apply, _.value)
  )
}