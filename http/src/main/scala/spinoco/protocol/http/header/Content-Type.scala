package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{ContentType, HeaderCodecDefinition}



/**
  *   RFC 7231 section 3.1.1.5
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Type
  */
sealed case class `Content-Type`(value: ContentType) extends DefaultHeader


object `Content-Type` { val codec =
  HeaderCodecDefinition[`Content-Type`](ContentType.codec.xmap (`Content-Type`.apply, _.value))
}
