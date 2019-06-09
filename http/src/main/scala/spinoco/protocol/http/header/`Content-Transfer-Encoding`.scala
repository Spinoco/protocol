package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.HeaderCodecDefinition
import spinoco.protocol.mime.TransferEncoding

/**
  * https://tools.ietf.org/html/rfc2045#section-6
  *
  * Although this does not seem to be HTTP header, in https://tools.ietf.org/html/rfc7578#section-4.5 is shown that it can
  * be used together with form data.
  *
  */
case class `Content-Transfer-Encoding`(value: TransferEncoding) extends DefaultHeader with ContentHeaderField

object `Content-Transfer-Encoding` {

  val codec = HeaderCodecDefinition.contentField[`Content-Transfer-Encoding`](TransferEncoding.codec.xmap(`Content-Transfer-Encoding`.apply, _.value))

}