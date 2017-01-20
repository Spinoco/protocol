package spinoco.protocol.http.header.value

import java.nio.charset.StandardCharsets

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.codec.helper._

/**
  * Created by pach on 12/01/17.
  */
sealed trait LanguageRange { self =>
  import LanguageRange._
  def qValue: Option[Float]

  def updateQValue(qValue:Option[Float]): LanguageRange = self match {
    case _: Any => Any(qValue)
    case one: One => one.copy(qValue = qValue)
  }

}

object LanguageRange {

  sealed case class Any(qValue:Option[Float]) extends LanguageRange
  sealed case class One(language:Language, qValue: Option[Float]) extends LanguageRange

  val `*` = Any(None)


  val codec : Codec[LanguageRange] = {
    val lrCodec:Codec[LanguageRange] = {
      choice(
        starCodec.xmap[Any](_ => `*`, _ => ()).upcast
        , Language.codec.xmap[One](l => One(l, None), l => l.language).upcast
      )
    }

    parametrized(semicolon, lrCodec, qValueCodec).xmap(
     { case (lr, qv) => lr.updateQValue(qv) }
      , lr => lr -> lr.qValue
    )
  }

}


sealed case class Language(tag: String, subTag: Option[String])

object Language {

  val codec : Codec[Language] = {
    implicit val chs = StandardCharsets.US_ASCII
    parametrized(dash,string,string).xmap(
      { case (tag, subTag) => Language(tag.trim, subTag.map(_.trim)) }
      , l => l.tag.trim -> l.subTag.map(_.trim)
    )
  }

}
