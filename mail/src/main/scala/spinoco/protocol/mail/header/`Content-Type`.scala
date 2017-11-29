package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mime.ContentType

case class `Content-Type`(tpe: ContentType) extends ContentHeaderField with DefaultEmailHeaderField

object `Content-Type` extends DefaultContentHeaderFieldDescription[`Content-Type`] {
  def codec: Codec[`Content-Type`] =
    ContentType.codec.xmap(`Content-Type`.apply, _.tpe)
}