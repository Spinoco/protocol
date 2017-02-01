package spinoco.protocol.http.header

import spinoco.protocol.http.Uri
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *  RFC 7231 5.5.2
  *
  *  @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referer
  */
case class Referer(uri:Uri) extends DefaultHeader

object Referer { val codec =
  HeaderCodecDefinition[Referer](Uri.codec.xmap (Referer.apply, _.uri) )
}
