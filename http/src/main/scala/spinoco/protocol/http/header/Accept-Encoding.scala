package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpEncodingRange}
import spinoco.protocol.http.codec.helper.commaDelimitedMin

/**
  *   RFC 7231 section 5.3.4
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
  */
sealed case class `Accept-Encoding`(value: List[HttpEncodingRange]) extends DefaultHeader


object `Accept-Encoding` { val codec =
  HeaderCodecDefinition[`Accept-Encoding`]( commaDelimitedMin(HttpEncodingRange.codec, 1).xmap (`Accept-Encoding`.apply,_.value) )
}