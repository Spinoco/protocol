package spinoco.protocol.mail.mime

import shapeless.Typeable
import spinoco.protocol.mime._
import spinoco.protocol.mail.header._

case class MIMEHeader(fields: List[ContentHeaderField]) { self =>

  def getField[A <: ContentHeaderField](implicit T: Typeable[A]): Option[A] =
    fields.collectFirst(Function.unlift(T.cast))

  def appendField(field: ContentHeaderField): MIMEHeader =
    self.copy(fields = self.fields :+ field)

  def getContentType: Option[ContentType] =
    getField[`Content-Type`].map(_.tpe)

  def getTransferEncoding: Option[TransferEncoding] =
    getField[`Content-Transfer-Encoding`].map(_.value)

  def getContentId: Option[String] =
    getField[`Content-ID`].map(_.id)

  def getContentDescription: Option[String] =
    getField[`Content-Description`].map(_.description)

  def getContentDisposition: Option[ContentDisposition] =
    getField[`Content-Disposition`].map(_.disposition)

  def contentType(tpe: ContentType): MIMEHeader =
    appendField(`Content-Type`(tpe))

  def transferEncoding(enc: TransferEncoding): MIMEHeader =
    appendField(`Content-Transfer-Encoding`(enc))

  def contentId(id: String): MIMEHeader =
    appendField(`Content-ID`(id))

  def contentDescription(desc: String): MIMEHeader =
    appendField(`Content-Description`(desc))

  def contentDisposition(disp: ContentDisposition): MIMEHeader =
    appendField(`Content-Disposition`(disp))

}









