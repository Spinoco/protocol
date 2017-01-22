package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{AgentVersion, HeaderCodecDefinition}


/**
  *   RFC 7231 5.5.3
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/User-Agent
  */
sealed case class `User-Agent`(value: AgentVersion) extends DefaultHeader

object `User-Agent` { val codec =
  HeaderCodecDefinition[`User-Agent`](AgentVersion.codec.xmap (`User-Agent`.apply, _.value))
}
