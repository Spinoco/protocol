package spinoco.protocol.http.header

import spinoco.protocol.common.codec._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

import scala.concurrent.duration._

/**
  *
  *   @see  https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Max-Age
  */
sealed case class `Access-Control-Max-Age`(value: FiniteDuration) extends DefaultHeader


object `Access-Control-Max-Age` { val codec =
  HeaderCodecDefinition[`Access-Control-Max-Age`](intAsString.xmap ( i => `Access-Control-Max-Age`(i.seconds),_.value.toSeconds.toInt))
}
