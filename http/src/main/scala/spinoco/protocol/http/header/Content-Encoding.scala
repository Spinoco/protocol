package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpEncoding}

/**
  *   RFC 7231 section 3.1.2.2
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding
  */
sealed case class `Content-Encoding`(value: HttpEncoding) extends DefaultHeader


object `Content-Encoding` { val codec =
  HeaderCodecDefinition[`Content-Encoding`](HttpEncoding.codec.xmap (`Content-Encoding`.apply, _.value))
}