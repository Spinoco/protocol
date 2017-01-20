package spinoco.protocol.http.header

import java.time.LocalDateTime

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition


/**
  *   RFC 7231 section 5.3
  *   @see  https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Expires
  */
sealed case class Expires(value: LocalDateTime) extends DefaultHeader


object Expires {  val codec =
  HeaderCodecDefinition[Expires](httpDateTimeCodec.xmap (Expires.apply, _.value))
}