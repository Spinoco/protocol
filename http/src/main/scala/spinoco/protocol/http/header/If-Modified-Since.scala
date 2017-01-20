package spinoco.protocol.http.header

import java.time.LocalDateTime

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *   RFC 7231 section 3.3
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Modified-Since
  */
sealed case class `If-Modified-Since`(value: LocalDateTime) extends DefaultHeader


object `If-Modified-Since` { val codec =
  HeaderCodecDefinition[`If-Modified-Since`](httpDateTimeCodec.xmap (`If-Modified-Since`.apply, _.value))
}

