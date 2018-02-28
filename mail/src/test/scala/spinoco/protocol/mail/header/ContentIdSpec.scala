package spinoco.protocol.mail.header

import org.scalacheck.Prop._
import org.scalacheck.Properties

object ContentIdSpec extends Properties("ContentId") {

  import spinoco.protocol.mail.SpecUtil._

  implicit val ContentIdCodec = `Content-ID`.codec

  property("conten-id") = protect {
    verify(
      "<image001.png@spnc.com>"
      , `Content-ID`("image001.png@spnc.com")
    )
  }

}
