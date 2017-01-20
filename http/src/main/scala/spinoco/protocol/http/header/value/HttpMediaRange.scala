package spinoco.protocol.http.header.value

import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import spinoco.protocol.http.codec.helper._

sealed trait HttpMediaRange { self =>
  import HttpMediaRange._

  def qValue: Option[Float]

  def updateQValue(qValue:Option[Float]): HttpMediaRange = self match {
    case pattern: Pattern => pattern.copy(qValue = qValue)
    case one: One => one.copy(qValue = qValue)
  }

}


object HttpMediaRange {

  sealed case class Pattern(mainType: String, qValue: Option[Float]) extends HttpMediaRange

  sealed case class One(mediaType: MediaType, qValue: Option[Float])  extends HttpMediaRange

  val `*/*` = Pattern("*", None)
  val `application/*` = Pattern("application", None)
  val `audio/*` = Pattern("audio", None)
  val `image/*` = Pattern("image", None)
  val `message/*` = Pattern("message", None)
  val `multipart/*` = Pattern("multipart", None)
  val `text/*` = Pattern("text", None)
  val `video/*` = Pattern("video", None)




  val codec : Codec[HttpMediaRange] = {

    val patternCodec: Codec[Pattern] = {
      tuple(slash, trimmedAsciiString, trimmedAsciiString).exmap(
        {
          case (tpe, "*") => Attempt.successful(Pattern(tpe, None))
          case (tpe, sub) => Attempt.failure(Err(s"Expected wildcard pattern, got $tpe/$sub"))
        }
        , p => Attempt.successful(p.mainType -> "*")
      )
    }

    val patternOrMedia: Codec[HttpMediaRange] = {
      choice(
        patternCodec.upcast
        , MediaType.codec.xmap[One](One(_,None),  _.mediaType).upcast
      )
    }

    parametrized(semicolon, patternOrMedia, qValueCodec).xmap(
      { case (mtr, qv) => mtr.updateQValue(qv) }
      , mtr => mtr -> mtr.qValue
    )

  }

}