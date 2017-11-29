package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.mime.MIMECharset


/**
  * Created by pach on 12/01/17.
  */
sealed trait HttpCharsetRange { self =>
  import HttpCharsetRange._

  def qValue: Option[Float]

  def updateQValue(qValue:Option[Float]): HttpCharsetRange = self match {
    case _: Any => Any(qValue)
    case one: One => one.copy(qValue = qValue)
  }

}

object HttpCharsetRange {

  sealed case class Any(qValue: Option[Float]) extends HttpCharsetRange

  sealed case class One(charset: MIMECharset, qValue: Option[Float]) extends HttpCharsetRange

  val `*` = Any(None)

  val codec: Codec[HttpCharsetRange] = {
    val chsCodec:Codec[HttpCharsetRange] = {
      choice(
        starCodec.xmap[Any](_ => Any(None), _ => ()).upcast
        , MIMECharset.codec.xmap[One](enc => One(enc, None), _.charset).upcast
      )
    }

    parametrized(semicolon, chsCodec, qValueCodec).xmap(
      { case (chs, qv) => chs.updateQValue(qv) }
      , chs => chs -> chs.qValue
    )

  }

}
