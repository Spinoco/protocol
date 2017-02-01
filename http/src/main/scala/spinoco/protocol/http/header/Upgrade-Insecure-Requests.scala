package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.HeaderCodecDefinition
import spinoco.protocol.common.codec._

case class `Upgrade-Insecure-Requests`(value:Int) extends DefaultHeader

object `Upgrade-Insecure-Requests` { val codec =
  HeaderCodecDefinition[`Upgrade-Insecure-Requests`](intAsString.xmap (`Upgrade-Insecure-Requests`.apply,_.value) )
}
