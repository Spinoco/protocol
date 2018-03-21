package spinoco.protocol.mail.header

import org.scalacheck.Prop._
import org.scalacheck.Properties
import spinoco.protocol.mime.ContentType
import spinoco.protocol.mime.MediaType.MultipartMediaType

/**
  * Created with IntelliJ IDEA.
  * User: raulim
  * Date: 5.3.18
  */
object ContentTypeSpec extends Properties("ContentType") {

  import spinoco.protocol.mail.SpecUtil._

  implicit val ContentTypeCodec = `Content-Type`.codec

  property("simple") = protect {
    verify(
      "multipart/mixed; Boundary=\"0__=4EBB08D4DFA584BB8f9e8a93df938690918c4EBB08D4DFA584BB\""
      , `Content-Type`(ContentType.MultiPartContent(MultipartMediaType("mixed", Map("boundary" -> "0__=4EBB08D4DFA584BB8f9e8a93df938690918c4EBB08D4DFA584BB")), None))
      , "multipart/mixed; boundary=\"0__=4EBB08D4DFA584BB8f9e8a93df938690918c4EBB08D4DFA584BB\""
    )
  }

  property("report") = protect {
    verify(
      "multipart/report; boundary=\"----=_Part_389928_767501270.1520332185688\"; report-type=delivery-status"
      , `Content-Type`(ContentType.MultiPartContent(MultipartMediaType("report", Map("boundary" -> "----=_Part_389928_767501270.1520332185688", "report-type" -> "delivery-status")), None))
      , "multipart/report; boundary=\"----=_Part_389928_767501270.1520332185688\"; report-type=\"delivery-status\""
    )
  }

  property("signed") = protect {
    verify(
      "multipart/signed; protocol=\"application/x-pkcs7-signature\"; micalg=SHA1; boundary=\"----_NextPart_201803092105523712.SIGNED\""
      , `Content-Type`(ContentType.MultiPartContent(MultipartMediaType("signed", Map("protocol" -> "application/x-pkcs7-signature", "micalg" -> "SHA1", "boundary" -> "----_NextPart_201803092105523712.SIGNED")), None))
      , "multipart/signed; protocol=\"application/x-pkcs7-signature\"; micalg=\"SHA1\"; boundary=\"----_NextPart_201803092105523712.SIGNED\""
    )
  }

}
