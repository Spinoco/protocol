package spinoco.protocol.http.header

import java.time.LocalDateTime

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition



/**
  *   RFC 7231 section 7.1.1.2
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date
  */
sealed case class Date(value: LocalDateTime) extends DefaultHeader

object Date {  val codec =
  HeaderCodecDefinition[Date](httpDateTimeCodec.xmap (Date.apply, _.value))
}

