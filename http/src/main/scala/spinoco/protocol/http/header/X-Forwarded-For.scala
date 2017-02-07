package spinoco.protocol.http.header

import spinoco.protocol.http.HostPort
import spinoco.protocol.http.codec.helper.commaDelimitedMin
import spinoco.protocol.http.header.value.HeaderCodecDefinition


case class `X-Forwarded-For`(hosts: List[HostPort]) extends DefaultHeader


object `X-Forwarded-For` { val codec =
  HeaderCodecDefinition[`X-Forwarded-For`](
    commaDelimitedMin(HostPort.codec, 1)
    .xmap (`X-Forwarded-For`.apply,_.hosts)
  )
}