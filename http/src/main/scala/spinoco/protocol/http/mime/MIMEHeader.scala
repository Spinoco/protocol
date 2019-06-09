package spinoco.protocol.http.mime

import shapeless.Typeable
import spinoco.protocol.http.header._
import spinoco.protocol.mime.{ContentDisposition, ContentType, TransferEncoding}


/**
  * The header for HTTP multipart data.
  *
  * @param fields The header fields.
  */
case class MIMEHeader(fields: List[ContentHeaderField]) { self =>

  def getField[A <: ContentHeaderField](implicit T: Typeable[A]): Option[A] =
    fields.collectFirst(Function.unlift(T.cast))

  def appendField(field: ContentHeaderField): MIMEHeader =
    self.copy(fields = self.fields :+ field)

  def getContentType: Option[ContentType] =
    getField[`Content-Type`].map(_.value)

  def getTransferEncoding: Option[TransferEncoding] =
    getField[`Content-Transfer-Encoding`].map(_.value)

  def getContentDisposition: Option[ContentDisposition] =
    getField[`Content-Disposition`].map(_.value)

  def contentType(tpe: ContentType): MIMEHeader =
    appendField(`Content-Type`(tpe))

  def transferEncoding(enc: TransferEncoding): MIMEHeader =
    appendField(`Content-Transfer-Encoding`(enc))

  def contentDisposition(disp: ContentDisposition): MIMEHeader =
    appendField(`Content-Disposition`(disp))

}






