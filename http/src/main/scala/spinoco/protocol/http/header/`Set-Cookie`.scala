package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpCookie}

/**
  *   RFC 6265 section 4.1
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie
  */
sealed case class `Set-Cookie`(value: HttpCookie) extends DefaultHeader


object `Set-Cookie` { val codec =
  HeaderCodecDefinition[`Set-Cookie`](HttpCookie.codec.xmap (`Set-Cookie`.apply, _.value))
}