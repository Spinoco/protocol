package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mail.header.codec._

/**
  * Created by pach on 17/10/17.
  */
case class Keywords(
  keyword: String
  , other: List[String]
) extends DefaultEmailHeaderField


object Keywords extends DefaultHeaderDescription[Keywords] {
  val codec: Codec[Keywords] = {
    commaSeparated(keyword, fold = false).xmap(
      Keywords.apply _ tupled, kw => (kw.keyword, kw.other)
    )
  }
}
