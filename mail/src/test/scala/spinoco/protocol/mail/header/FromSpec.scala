package spinoco.protocol.mail.header

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spinoco.protocol.mail.EmailAddress

/**
  * Created by pach on 18/10/17.
  */
object FromSpec extends Properties("From") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = From.codec

  property("single-email") = protect {

    verify(
      "John Doe <john.doe@spinoco.com>"
      , From(EmailAddress("john.doe", "spinoco.com", Some("John Doe")), Nil)
      , "\"John Doe\" <john.doe@spinoco.com>"
    )

  }

  property("multiple-email") = protect {
    verify(
      "\"John Doe\" <john.doe@spinoco.com>, jannet.doe@spinoco.com"
      , From(EmailAddress("john.doe", "spinoco.com", Some("John Doe")), List(EmailAddress("jannet.doe", "spinoco.com", None)))
      , "\"John Doe\" <john.doe@spinoco.com>,\r\n jannet.doe@spinoco.com"
    )

  }

}
