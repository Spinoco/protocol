package spinoco.protocol.http.header.value

import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import spinoco.protocol.common.Terminator
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.mime.MediaType
import spinoco.protocol.common.codec._

sealed trait HttpMediaRange { self =>
  import HttpMediaRange._

  def qValue: Option[Float]

  def params: Map[String, String]

  def acceptParams: Map[String, String]

  def updateQValue(qValue:Option[Float]): HttpMediaRange = self match {
    case pattern: Pattern => pattern.copy(qValue = qValue)
    case one: One => one.copy(qValue = qValue)
  }

  def updateParams(params: Map[String, String]): HttpMediaRange = self match {
    case pattern: Pattern => pattern.copy(params = params)
    case one: One => one.copy(params = params)
  }

  def updateAcceptParams(params: Map[String, String]): HttpMediaRange = self match {
    case pattern: Pattern => pattern.copy(acceptParams = params)
    case one: One => one.copy(acceptParams = params)
  }

}


object HttpMediaRange {

  sealed case class Pattern(
    mainType: String
    , qValue: Option[Float]
    , params: Map[String, String] = Map.empty
    , acceptParams: Map[String, String]  = Map.empty
  ) extends HttpMediaRange

  sealed case class One(
    mediaType: MediaType
    , qValue: Option[Float]
    , params: Map[String, String] = Map.empty
    , acceptParams: Map[String, String] = Map.empty
  )  extends HttpMediaRange

  val `*/*` = Pattern("*", None)
  val `application/*` = Pattern("application", None)
  val `audio/*` = Pattern("audio", None)
  val `image/*` = Pattern("image", None)
  val `message/*` = Pattern("message", None)
  val `multipart/*` = Pattern("multipart", None)
  val `text/*` = Pattern("text", None)
  val `video/*` = Pattern("video", None)




  val codec : Codec[HttpMediaRange] = {

    val parameter: Codec[(String, String)] = {
      terminated(trimmedAsciiToken, Terminator.constantString1("=")) ~ trimmedAsciiToken
    }

    val wildcardCodec: Codec[Pattern] = {
      asciiStringNoWs.exmap(
        s => {
          if (s == "*") Attempt.successful(Pattern("*", None)) else
          Attempt.failure(Err(s"Expected wildcard pattern (*), got $s"))
        }
        , p => {
          if (p.mainType == "*") Attempt.successful("*")
          else Attempt.failure(Err(s"Expected wildcard pattern, got $p"))
        }

      )
    }

    val patternCodec: Codec[Pattern] = {
      (terminated(trimmedAsciiToken, Terminator.constantString1("/")) ~ trimmedAsciiToken)
      .exmap(
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
        , wildcardCodec.upcast
        , MediaType.codec.xmap[One](One(_,None),  _.mediaType).upcast
      )
    }

    val patterMediaWithParams: Codec[HttpMediaRange] = {
      parametrized(semicolon, patternOrMedia <~ ignoreWS, delimitedBy(semicolon, semicolon, parameter)).xmap(
        {case (mtr, pars) => mtr.updateParams(pars.toList.flatten.toMap)}
        , mtr => (mtr, mtr.params.headOption.map(_ => mtr.params.toList))
      )
    }

    val qAccepts = {
      floatAsString ~ maybe(ignoreWS ~> constant(semicolon) ~> delimitedBy(semicolon, semicolon, parameter))
    }


    choice(
      terminated(patterMediaWithParams, Terminator.constant1((semicolon ++ qPar).bits)) ~ qAccepts.widen[Option[(Float, Option[List[(String, String)]])]](a => Some(a), {
        case None => Attempt.failure(Err("---"))
        case Some(v) => Attempt.successful(v)
      })
      , terminated(patterMediaWithParams, Terminator.constant1((semicolon_SP ++ qPar).bits)) ~ qAccepts.widen[Option[(Float, Option[List[(String, String)]])]](a => Some(a), {
        case None => Attempt.failure(Err("---"))
        case Some(v) => Attempt.successful(v)
      })
      , patterMediaWithParams.xmap[(HttpMediaRange, Option[(Float, Option[List[(String, String)]])])](mtr => mtr -> None, {case (mtr, _) => mtr})
    ).xmap[HttpMediaRange](
      { case (mtr, qv) => mtr.updateQValue(qv.map(_._1)).updateAcceptParams(qv.flatMap(_._2).toList.flatten.toMap) }
      , mtr => mtr -> mtr.qValue.map(_ -> mtr.acceptParams.headOption.map(_ => mtr.acceptParams.toList))
    )
  }

}