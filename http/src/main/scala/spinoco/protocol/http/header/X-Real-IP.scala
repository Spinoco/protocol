package spinoco.protocol.http.header

import spinoco.protocol.http.HostPort
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  * Created by pach on 07/02/17.
  */
case class `X-Real-IP`(host:HostPort) extends DefaultHeader


object `X-Real-IP` { val codec =
  HeaderCodecDefinition[`X-Real-IP`](HostPort.codec.xmap (`X-Real-IP`.apply,_.host))
}