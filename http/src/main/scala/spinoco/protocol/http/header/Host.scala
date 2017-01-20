package spinoco.protocol.http.header

import spinoco.protocol.http.HostPort
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  *   RFC 7231 section 5.4
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Host
  */
sealed case class Host(value: HostPort) extends DefaultHeader


object Host {  val codec =
  HeaderCodecDefinition[Host](HostPort.codec.xmap (Host.apply, _.value))
}


