package spinoco.protocol.http

import scodec.Codec
import codec.helper._
import spinoco.protocol.common.codec._

sealed case class HostPort(host: String, port: Option[Int])


object HostPort {

  val codec: Codec[HostPort] = {

    parametrized[String, Int](colon, trimmedAsciiToken, intAsString)
    .xmap(
      HostPort.apply _ tupled
      , hp => hp.host -> hp.port
    )

  }




}