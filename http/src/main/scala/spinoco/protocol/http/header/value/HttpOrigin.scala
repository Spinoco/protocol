package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.HostPort
import spinoco.protocol.common.codec._
import spinoco.protocol.common.Terminator


sealed trait HttpOrigin

object HttpOrigin {

  case object Any extends HttpOrigin

  sealed case class One(scheme: String, host: Option[HostPort]) extends HttpOrigin

  val oneCodec: Codec[One] = {
    (ignoreWS ~> terminated(asciiToken, Terminator.constantString1("://")) ~ orEmpty(HostPort.codec))
      .xmap[One](One.apply _ tupled, one => one.scheme -> one.host)
      .withToString("HttpOrigin.One")
  }

  val codec : Codec[HttpOrigin] = {
    choice(
      starCodec.xmap[HttpOrigin.Any.type](_ => HttpOrigin.Any, _ => ()).upcast
      , oneCodec.upcast
    )
  }


}

