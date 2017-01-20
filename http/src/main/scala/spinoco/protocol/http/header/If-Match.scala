package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{EntityTagRange, HeaderCodecDefinition}

/**
  *   RFC 7231 section 3.1
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Match
  */
sealed case class `If-Match`(value: EntityTagRange) extends DefaultHeader



object `If-Match` {  val codec =
  HeaderCodecDefinition[`If-Match` ](EntityTagRange.codec.xmap (`If-Match`.apply, _.value))
}