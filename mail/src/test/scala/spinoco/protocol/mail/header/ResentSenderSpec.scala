package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import spinoco.protocol.mail.EmailAddress

/**
  * Created by pach on 23/10/17.
  */
object ResentSenderSpec extends Properties("ResentSender") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = `Resent-Sender`.codec


  property("single-email") = protect {

    verify(
      "John Doe <john.doe@spinoco.com>"
      , `Resent-Sender`(EmailAddress("john.doe", "spinoco.com", Some("John Doe")))
    )

  }


}