package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpChallenge}

/**
  *   RFC 2617
  *   @see https://tools.ietf.org/html/rfc2617
  */
sealed case class `WWW-Authenticate`(value: HttpChallenge) extends DefaultHeader


object `WWW-Authenticate` { val codec =
  HeaderCodecDefinition[`WWW-Authenticate`](HttpChallenge.codec.xmap (`WWW-Authenticate`.apply, _.value))
}

