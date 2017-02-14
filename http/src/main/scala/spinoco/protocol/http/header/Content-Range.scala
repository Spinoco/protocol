package spinoco.protocol.http.header

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.header.value.HeaderCodecDefinition
import spinoco.protocol.common.codec._



sealed case class `Content-Range`(from: Long, to: Long, total: Option[Long]) extends DefaultHeader

object `Content-Range` {

  private val scodec: Codec[`Content-Range`] = {
    import spinoco.protocol.http.codec.helper._

    val maybeTotal: Codec[Option[Long]] = choice(
      asciiConstant("*").xmap[None.type](_ => None, _ => ()).upcast
      , longAsString.xmap[Some[Long]](l => Some(l), { case Some(l) => l }).upcast
    )
    val fromTo: Codec[(Long, Long)] =
      tuple(dash, longAsString, longAsString)

    whitespace("") ~> asciiConstant("bytes", ignoreCase = true) ~> whitespace() ~>
    tuple(slash, fromTo, maybeTotal).xmap (
      { case ((from, to), maybeTotal) => `Content-Range`(from, to, maybeTotal) }
      , cr => ((cr.from, cr.to), cr.total)
    )
  }

  val codec = HeaderCodecDefinition[`Content-Range`](scodec)
}