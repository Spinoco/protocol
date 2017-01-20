package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.codec.helper._


sealed trait HttpEncodingRange { self =>
  import HttpEncodingRange._

  def qValue: Option[Float]

  def updateQValue(qValue:Option[Float]): HttpEncodingRange = self match {
    case _: Any => Any(qValue)
    case one: One => one.copy(qValue = qValue)
  }

}

object HttpEncodingRange {

  sealed case class Any(qValue: Option[Float]) extends HttpEncodingRange

  sealed case class One(encoding: HttpEncoding, qValue: Option[Float]) extends HttpEncodingRange

  val codec : Codec[HttpEncodingRange] = {
    val encCodec:Codec[HttpEncodingRange] = {
      choice(
        starCodec.xmap[Any](_ => Any(None), _ => ()).upcast
        , HttpEncoding.codec.xmap[One](enc => One(enc, None), _.encoding).upcast
      )
    }

    parametrized(semicolon, encCodec, qValueCodec).xmap(
      { case (enc, qv) => enc.updateQValue(qv) }
      , enc => enc -> enc.qValue
    )
  }
}
