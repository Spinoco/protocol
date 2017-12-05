package spinoco.protocol.email.header

import scodec.Codec
import spinoco.protocol.email.header.codec._

/**
  * Created by pach on 17/10/17.
  */
case class Keywords(
  keyword: String
  , other: List[String]
) extends EmailHeaderField {
  def name: String = Keywords.name
}


object Keywords extends HeaderDescription[Keywords] {

  val name: String = "Keywords"

  val codec: Codec[Keywords] = {
    commaSeparated(keyword, fold = false).xmap(
      Keywords.apply _ tupled, kw => (kw.keyword, kw.other)
    )
  }

  def fieldCodec: Codec[EmailHeaderField] = codec.upcast



}
