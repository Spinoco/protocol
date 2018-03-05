package spinoco.protocol.mail.header

/**
  * Created with IntelliJ IDEA.
  * User: raulim
  * Date: 5.3.18
  */

/**
  * Value of known header is not RFC compliant
  * Only for decoding of incoming emails.
  */
case class NonRFC(name: String, value: String) extends EmailHeaderField with ContentHeaderField
