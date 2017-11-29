package spinoco.protocol.mail.header

import scodec.Codec
import spinoco.protocol.mail.mime.TransferEncoding

case class `Content-Transfer-Encoding`(value: TransferEncoding)
  extends  ContentHeaderField with DefaultEmailHeaderField

object `Content-Transfer-Encoding` extends DefaultContentHeaderFieldDescription[`Content-Transfer-Encoding`] {
  def codec: Codec[`Content-Transfer-Encoding`] =
    TransferEncoding.codec.xmap(`Content-Transfer-Encoding`.apply, _.value)
}