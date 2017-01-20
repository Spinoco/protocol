package spinoco.protocol.http.header

import java.time.LocalDateTime

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *   RFC 7232 appendix 2.2
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Last-Modified
  */
sealed case class `Last-Modified`(value: LocalDateTime) extends DefaultHeader


object `Last-Modified` { val codec =
  HeaderCodecDefinition[`Last-Modified`](httpDateTimeCodec.xmap (`Last-Modified`.apply, _.value))
}