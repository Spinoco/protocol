package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{EntityTag, HeaderCodecDefinition}

/**
  *   RFC 7231 section 2.3
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag
  */
sealed case class ETag(value: EntityTag) extends DefaultHeader



object ETag { val codec =
  HeaderCodecDefinition[ETag](EntityTag.codec.xmap (ETag.apply, _.value))
}
