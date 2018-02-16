package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import spinoco.protocol.mail.EmailAddress

/**
  * Created by pach on 23/10/17.
  */
object ResentDestinationSpec extends Properties("ResentDestination") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = ResentDestination.To.codec

  property("single-email") = protect {

    verify(
      "John Doe <john.doe@spinoco.com>"
      , ResentDestination(DestinationType.To, EmailAddress("john.doe", "spinoco.com", Some("John Doe")), Nil)
      , "\"John Doe\" <john.doe@spinoco.com>"
    )

  }

  property("multiple-email") = protect {
    verify(
      "\"John Doe\" <john.doe@spinoco.com>, jannet.doe@spinoco.com"
      , ResentDestination(DestinationType.To, EmailAddress("john.doe", "spinoco.com", Some("John Doe")), List(EmailAddress("jannet.doe", "spinoco.com", None)))
      , "\"John Doe\" <john.doe@spinoco.com>,\r\n jannet.doe@spinoco.com"
    )

  }

}
