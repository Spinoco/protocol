package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpCharsetRange}
import spinoco.protocol.http.codec.helper.commaDelimitedMin

/**
  *   RFC 7231 section 5.3.3
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Charset
  */
sealed case class `Accept-Charset`(value: List[HttpCharsetRange]) extends DefaultHeader

object `Accept-Charset` { val codec =
  HeaderCodecDefinition[`Accept-Charset`]( commaDelimitedMin(HttpCharsetRange.codec, 1).xmap (`Accept-Charset`.apply,_.value) )
}