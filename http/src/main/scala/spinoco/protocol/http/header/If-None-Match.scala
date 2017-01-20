package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{EntityTagRange, HeaderCodecDefinition}

/**
  *   RFC 7231 section 3.2
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-None-Match
  */
sealed case class `If-None-Match`(value: EntityTagRange) extends DefaultHeader


object `If-None-Match` {  val codec =
  HeaderCodecDefinition[`If-None-Match`](EntityTagRange.codec.xmap (`If-None-Match`.apply, _.value))
}

