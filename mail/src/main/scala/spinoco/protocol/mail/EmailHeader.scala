package spinoco.protocol.mail


import java.time.ZonedDateTime

import shapeless.Typeable
import spinoco.protocol.mail.header._
import spinoco.protocol.mime.{ContentType, TransferEncoding}

/**
  * Represents Email header that consists of arbitrary fields.
  *
  * Encoding/Decoding of this header is according to specification in RFC 5322.
  *
  */
case class EmailHeader(fields: List[EmailHeaderField]) { self =>

  /** gets any fields of type `A` **/
  def get[A <: EmailHeaderField](implicit T: Typeable[A]): List[A] =
    fields.collect(Function.unlift(T.cast))

  def from: List[EmailAddress] =
    get[From] map { _.email }

  private def destination(ofType: DestinationType.Value): List[EmailAddress] =
    get[Destination].filter(_.tpe == ofType) flatMap { d => d.email +: d.others }

  private def replaceField[A <: EmailHeaderField](field: A)(implicit T: Typeable[A]) = {
    self.copy(fields = fields.filterNot(fld => T.cast(fld).nonEmpty) :+ field)
  }


  /** returns any email address marked as `to` **/
  def to: Seq[EmailAddress] = destination(DestinationType.To)

  /** returns any email addresses marked as `cc` **/
  def cc: Seq[EmailAddress] = destination(DestinationType.Cc)

  /** returns any email addresses marked as `bcc` **/
  def bcc: Seq[EmailAddress] = destination(DestinationType.Bcc)

  /** add supplied fields to header **/
  def add(field1: EmailHeaderField, fields: EmailHeaderField*): EmailHeader =
    self.copy(fields = self.fields ++ ( field1 +: fields))

  /** adds `from` email destination **/
  def addFrom(address1: EmailAddress, addresses: EmailAddress*): EmailHeader = {
    get[From].headOption match {
      case None => add(From(address1, addresses.toList))
      case Some(from) =>
        self.copy(fields = self.fields.filter(_ != from))
        .add(from.copy(others = from.others ++ (address1 +: addresses)))
    }
  }

  /** adds destination. If header already exists, it is updated **/
  private def addDestination(ofType: DestinationType.Value, address1: EmailAddress, addresses: EmailAddress*): EmailHeader = {
    get[Destination].find(_.tpe == ofType) match {
      case None => add(Destination(ofType, address1, addresses.toList))
      case Some(dest) =>
        self.copy(fields = self.fields.filter(_ != dest))
        .add(dest.copy(others = dest.others ++ (address1 +: addresses)))
    }
  }

  /** adds `to` address(es) **/
  def addTo(address1: EmailAddress, addresses: EmailAddress*) : EmailHeader =
    addDestination(DestinationType.To, address1, addresses:_*)

  /** adds `cc` address(es) **/
  def addCc(address1: EmailAddress, addresses: EmailAddress*) : EmailHeader =
    addDestination(DestinationType.Cc, address1, addresses:_*)

  /** adds `bcc` address(es) **/
  def addBcc(address1: EmailAddress, addresses: EmailAddress*) : EmailHeader =
    addDestination(DestinationType.Bcc, address1, addresses:_*)

  /** adds supplied contenttype replacing any previously found content types **/
  def contentType(contentType: ContentType): EmailHeader =
    replaceField(`Content-Type`(contentType))

  /** sets transfer encoding to supplied encoding **/
  def contentTransferEncoding(transferEncoding: TransferEncoding) =
    replaceField(`Content-Transfer-Encoding`(transferEncoding))


}


object EmailHeader {

  /** builds standard email header from subject and date when the emaill will be marked as originated **/
  def apply(
    subject: String
    , date: ZonedDateTime
    , from: EmailAddress
    , to: EmailAddress
  ): EmailHeader = {
    EmailHeader(List(
      Subject(subject)
      , OriginationDate(date)
      , From(from, Nil)
      , Destination(DestinationType.To, to, Nil)
    ))
  }

}
