package spinoco.protocol.http.header.value

import scodec.Codec
import spinoco.protocol.http.codec.helper._


sealed case class AgentVersion(version:String)

object AgentVersion {
  val codec: Codec[AgentVersion] =
    trimmedUtf8String.xmap(AgentVersion.apply, _.version)
}
