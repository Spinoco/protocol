package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, ServerProduct}


/**
  *   RFC 7231 section 7.4.2
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Server
  */
sealed case class Server(value: ServerProduct) extends DefaultHeader


object Server { val codec =
  HeaderCodecDefinition[Server](ServerProduct.codec.xmap (Server.apply, _.value))
}
