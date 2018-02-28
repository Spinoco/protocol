package spinoco.protocol.mail.header

import scodec.codecs._
import spinoco.protocol.common.codec._

case class `Content-ID`(id: String)
  extends DefaultEmailHeaderField with ContentHeaderField

object `Content-ID` extends DefaultContentHeaderFieldDescription[`Content-ID`] {
  val codec = (constantString1("<") ~> takeWhileChar(ascii)('>') <~ constantString1(">")).xmap[`Content-ID`](`Content-ID`.apply, _.id)
}
