package spinoco.protocol.email.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import spinoco.protocol.email.EmailAddress

/**
  * Created by pach on 23/10/17.
  */
object ResentSenderSpec extends Properties("ResentSender") {

  import spinoco.protocol.email.SpecUtil._
  implicit val HeaderCodec = ResentSender.codec


  property("single-email") = protect {

    verify(
      "John Doe <john.doe@spinoco.com>"
      , ResentSender(EmailAddress("john.doe", "spinoco.com", Some("John Doe")))
    )

  }


}