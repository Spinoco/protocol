package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpOrigin}
import spinoco.protocol.http.codec.helper.commaDelimitedMin

/**
  *   RFC 6454 section 7
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Origin
  */
sealed case class Origin(value: List[HttpOrigin.One]) extends DefaultHeader


object Origin { val codec =
  HeaderCodecDefinition[Origin](commaDelimitedMin(HttpOrigin.oneCodec, 1).xmap (Origin.apply, _.value))
}


