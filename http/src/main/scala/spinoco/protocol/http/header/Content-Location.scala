package spinoco.protocol.http.header

import spinoco.protocol.http.Uri
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *   RFC 7231 section 3.1.4.2
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Location
  */
sealed case class `Content-Location`(value: Uri.Path) extends DefaultHeader


object `Content-Location` { val codec =
  HeaderCodecDefinition[`Content-Location`](Uri.Path.codec.xmap (`Content-Location`.apply, _.value))
}

