package spinoco.protocol.http.header.value

import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import spinoco.protocol.http.codec.helper._

sealed case class EntityTag(tag: String, weak: Boolean)


object EntityTag {


  val codec: Codec[EntityTag] = {
    val weakTag: Codec[EntityTag] = {
      (asciiConstant("W/") ~> alwaysQuotedUtf8String).exmap(
        s => Attempt.successful(EntityTag(s, weak = true))
        , et => if (et.weak) Attempt.successful(et.tag) else Attempt.failure(Err("Weak tag is expected"))
      )
    }

    choice(
      weakTag
      , alwaysQuotedUtf8String.xmap(s => EntityTag(s, weak = false), _.tag)
    )
  }

}