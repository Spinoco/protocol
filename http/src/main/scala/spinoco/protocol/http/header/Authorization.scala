package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpCredentials}

/**
  * Authorization header,
  *  RFC 2617
  */
case class Authorization(credentials: HttpCredentials) extends DefaultHeader

object Authorization {val codec =
  HeaderCodecDefinition[Authorization](HttpCredentials.codec.xmap (Authorization.apply, _.credentials))
}
