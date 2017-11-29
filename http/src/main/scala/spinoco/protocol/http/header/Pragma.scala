package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition

/**
  * Created by pach on 24/01/17.
  */
case class Pragma(value:String) extends DefaultHeader

object Pragma { val codec =
  HeaderCodecDefinition[Pragma](trimmedAsciiToken.xmap (Pragma.apply,_.value) )
}
