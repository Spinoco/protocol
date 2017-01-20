package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.{HeaderCodecDefinition, HttpMediaRange}

/**
  *   RFC 7231 section 5.3.2
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept
  */
sealed case class Accept(value:List[HttpMediaRange]) extends DefaultHeader



object Accept { val codec =
  HeaderCodecDefinition[Accept]( commaDelimited(HttpMediaRange.codec).xmap (Accept.apply,_.value) )
}