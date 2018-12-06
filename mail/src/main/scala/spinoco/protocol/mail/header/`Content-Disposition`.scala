package spinoco.protocol.mail.header

import spinoco.protocol.mime.ContentDisposition

case class `Content-Disposition`(disposition: ContentDisposition) extends DefaultEmailHeaderField with ContentHeaderField

object `Content-Disposition` extends DefaultContentHeaderFieldDescription[`Content-Disposition`] {
  val codec = ContentDisposition.emailCodec.xmap[`Content-Disposition`](`Content-Disposition`.apply, _.disposition)
}