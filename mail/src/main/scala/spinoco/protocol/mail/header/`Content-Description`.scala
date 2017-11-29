package spinoco.protocol.mail.header

case class `Content-Description`(description: String) extends DefaultEmailHeaderField with ContentHeaderField

object `Content-Description` extends DefaultContentHeaderFieldDescription[`Content-Description`] {
  val codec = scodec.codecs.utf8.xmap[`Content-Description`](`Content-Description`.apply, _.description)
}
