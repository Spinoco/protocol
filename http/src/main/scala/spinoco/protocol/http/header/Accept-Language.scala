package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, LanguageRange}
import spinoco.protocol.http.codec.helper.commaDelimitedMin


/**
  *   RFC 7231 section 5.3.5
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Language
  */
sealed case class `Accept-Language`(value: List[LanguageRange]) extends DefaultHeader


object `Accept-Language`  { val codec =
  HeaderCodecDefinition[`Accept-Language`]( commaDelimitedMin(LanguageRange.codec, 1).xmap (`Accept-Language`.apply,_.value) )
}
