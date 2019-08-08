package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties

object AutoSubmittedSpec extends Properties("AutoSubmitted") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = `Auto-Submitted`.codec


  property("no") = protect {
    verify(
      "no"
      , `Auto-Submitted`(`Auto-Submitted`.AutoType.No)
    )
  }

  property("auto-generated") = protect {
    verify(
      "auto-generated"
      , `Auto-Submitted`(`Auto-Submitted`.AutoType.AutoGenerated)
    )
  }

  property("auto-replied") = protect {
    verify(
      "auto-replied"
      , `Auto-Submitted`(`Auto-Submitted`.AutoType.AutoReplied)
    )
  }

  property("auto-notified") = protect {
    verify(
      "auto-notified"
      , `Auto-Submitted`(`Auto-Submitted`.AutoType.AutoNotified)
    )
  }


}