package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, KeepAliveParams}

/**
  *   RFC 7230 appendix A.1.2
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Keep-Alive
  */
sealed case class `Keep-Alive`(value: KeepAliveParams) extends DefaultHeader


object `Keep-Alive` { val codec =
  HeaderCodecDefinition[`Keep-Alive`](KeepAliveParams.codec.xmap (`Keep-Alive`.apply, _.value))
}