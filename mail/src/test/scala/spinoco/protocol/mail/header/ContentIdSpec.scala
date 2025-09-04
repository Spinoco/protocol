package spinoco.protocol.mail.header

import org.scalacheck.Prop._
import org.scalacheck.Properties
import scodec.Codec

object ContentIdSpec extends Properties("ContentId") {

  import spinoco.protocol.mail.SpecUtil._

  implicit val ContentIdCodec: Codec[`Content-ID`] = `Content-ID`.codec

  property("content-id") = protect {
    verify(
      "<image001.png@spnc.com>"
      , `Content-ID`("image001.png@spnc.com")
    )
  }

}
