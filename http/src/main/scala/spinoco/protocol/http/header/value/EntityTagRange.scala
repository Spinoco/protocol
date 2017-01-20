package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.codec.helper._

/**
  * Created by pach on 12/01/17.
  */
sealed trait EntityTagRange

object EntityTagRange {

  case object Any extends EntityTagRange
  sealed case class Range(tags: List[EntityTag]) extends EntityTagRange

  val `*` = Any


  val codec: Codec[EntityTagRange] = {
    choice(
      asciiConstant("*").xmap[Any.type](_ => Any, _ => ()).upcast
      , commaDelimited(EntityTag.codec).xmap[Range](Range.apply, _.tags).upcast
    )
  }

}
