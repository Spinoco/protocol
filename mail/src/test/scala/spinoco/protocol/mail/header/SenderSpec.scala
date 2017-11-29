package spinoco.protocol.mail.header

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spinoco.protocol.mail.EmailAddress

/**
  * Created by pach on 18/10/17.
  */
object SenderSpec extends Properties("Sender") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = Sender.codec


  property("single-email") = protect {

    verify(
      "John Doe <john.doe@spinoco.com>"
      , Sender(EmailAddress("john.doe", "spinoco.com", Some("John Doe")))
    )

  }


}
