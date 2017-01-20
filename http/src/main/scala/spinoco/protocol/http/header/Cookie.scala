package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpCookie}

/**
  *   RFC 6265 section 5.3, 5.4
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cookie
  */
sealed case class Cookie(value: HttpCookie) extends DefaultHeader


object Cookie { val codec =
  HeaderCodecDefinition[Cookie](HttpCookie.codec.xmap (Cookie.apply, _.value))
}

