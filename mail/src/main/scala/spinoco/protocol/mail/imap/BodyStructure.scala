package spinoco.protocol.mail.imap

import java.time.LocalDate

import spinoco.protocol.mail.EmailAddress

object BodyStructure {





  /**
    * Body type fields
    * @param params       Parameters (arbitrary)
    * @param id           Id of the body
    * @param desc         Description of the body
    * @param encoding     Body encoding (i.e. BASE64)
    * @param size         Size of the body in octets
    */
  case class BodyFields(
    params: Vector[(String, String)]
    , id: Option[String]
    , desc: Option[String]
    , encoding: String
    , size: Int
  )

  /**
    * Message body type
    * @param fields     Fields
    * @param envelope   Message Envelope
    * @param body       Body of the message
    * @param lines      Lines of the message
    */
  case class BodyTypeMessage(
    fields: BodyFields
    , envelope: Envelope
    , body: BodyPart
    , lines: Int
  ) extends BodyType

  /**
    * Text Body type.
    * @param subType    SubType (i.e. PLAIN)
    * @param fields     Any fields for TEXT type body
    * @param lines      Number of lines for the body
    */
  case class BodyTypeText(
    subType: String
    , fields: BodyFields
    , lines: Int
  ) extends BodyType


  /**
    * Basic subtype (i.e. binary files)
    *
    * @param media      Media Type
    * @param fields     Fields
    */
  case class BodyTypeBasic(
    media: BasicMedia
    , fields: BodyFields
  ) extends BodyType

  /** type of body **/
  sealed trait BodyType


  /** Extension for single body parts **/
  case class SingleBodyExtension(
    md5: Option[String]
    , dsp: Option[(String, Vector[(String, String)])]
    , lang: Option[List[String]]
    , loc: Option[String]
    , extensions: Vector[BodyExtension]
  )

  /** Extension for multi body parts **/
  case class MultiBodyExtension(
    params: Vector[(String, String)]
    , dsp: Option[(String, Vector[(String, String)])]
    , lang: Option[List[String]]
    , loc: Option[String]
    , extensions: Vector[BodyExtension]
  )

  /** body extension with string value **/
  case class StringBodyExtension(ext: String) extends BodyExtension

  /** body extension with int value **/
  case class IntBodyExtension(ext: Int) extends BodyExtension

  /** multiple body extensions. vector is nonempty **/
  case class ListBodyExtension(ext: Vector[BodyExtension]) extends BodyExtension

  /** arbitrary extensions to the body **/
  sealed trait BodyExtension


  /** resposne to imap ENVELOPE command **/
  case class Envelope(
    date: Option[LocalDate]
    , subject: Option[String]
    , from: Vector[EmailAddress]
    , sender: Vector[EmailAddress]
    , replyTo: Vector[EmailAddress]
    , to: Vector[EmailAddress]
    , ccc: Vector[EmailAddress]
    , bcc: Vector[EmailAddress]
    , inReplyTo: Option[String]
    , messageId: Option[String]
  )


  /**
    * A Single Body Part
    * @param tpe  Type of Body
    * @param ext  Extensions if available
    */
  case class SingleBodyPart(
    tpe: BodyType
    , ext: Option[SingleBodyExtension]
  ) extends BodyPart

  /**
    * A multiple Body Part
    * @param parts            available Body parts, must be Non Empty.
    * @param mediaSubType     media subtypes
    * @param ext              Extension if available
    */
  case class MultiBodyPart(
    parts: Vector[BodyPart]
    , mediaSubType: String
    , ext: Option[MultiBodyExtension]
  ) extends BodyPart

  /** parts of the body **/
  sealed trait BodyPart

  /** simple media type and subtype **/
  case class BasicMedia(media: String, subType: String)


}
