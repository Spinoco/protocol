package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import spinoco.protocol.mail.EmailAddress

/**
  * Created by pach on 23/10/17.
  */
object ResentFromSpec extends Properties("ResentFrom") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = `Resent-From`.codec

  property("single-email") = protect {

    verify(
      "John Doe <john.doe@spinoco.com>"
      , `Resent-From`(EmailAddress("john.doe", "spinoco.com", Some("John Doe")), Nil)
    )

  }

  property("multiple-email") = protect {
    verify(
      "John Doe <john.doe@spinoco.com>, jannet.doe@spinoco.com"
      , `Resent-From`(EmailAddress("john.doe", "spinoco.com", Some("John Doe")), List(EmailAddress("jannet.doe", "spinoco.com", None)))
      , "John Doe <john.doe@spinoco.com>,\r\n jannet.doe@spinoco.com"
    )

  }

}
