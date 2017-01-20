package spinoco.protocol.http.header

import spinoco.protocol.common.codec._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

import scala.concurrent.duration._

/**
  *   RFC 7234 section 5.1
  *
  *   @see  https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Age
  */
sealed case class Age(value: FiniteDuration) extends DefaultHeader


object Age { val codec =
  HeaderCodecDefinition[Age](intAsString.xmap ( i => Age(i.seconds),_.value.toSeconds.toInt))
}