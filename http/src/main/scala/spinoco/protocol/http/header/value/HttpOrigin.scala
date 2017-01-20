package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.{HostPort, HttpScheme}


sealed trait HttpOrigin

object HttpOrigin {

  case object Any extends HttpOrigin

  sealed case class One(scheme: HttpScheme.Value, host: HostPort) extends HttpOrigin

  val oneCodec: Codec[One] = {
    tuple[HttpScheme.Value, HostPort](`://`, HttpScheme.codec, HostPort.codec)
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

