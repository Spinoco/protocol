package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{CacheDirective, HeaderCodecDefinition}
import spinoco.protocol.http.codec.helper.commaDelimitedMin

/**
  *   RFC 72314
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
  */
sealed case class `Cache-Control`(value: List[CacheDirective]) extends DefaultHeader


object `Cache-Control` { val codec =
  HeaderCodecDefinition[`Cache-Control`](commaDelimitedMin(CacheDirective.codec, 1).xmap (`Cache-Control`.apply, _.value))
}
