package spinoco.protocol.mail.header

case class `Content-ID`(id: String)
  extends DefaultEmailHeaderField with ContentHeaderField


object `Content-ID` extends DefaultContentHeaderFieldDescription[`Content-ID`] {
  val codec = scodec.codecs.ascii.xmap[`Content-ID`](`Content-ID`.apply, _.id)
}
