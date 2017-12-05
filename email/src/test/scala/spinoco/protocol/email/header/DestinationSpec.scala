package spinoco.protocol.email.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import spinoco.protocol.email.EmailAddress

/**
  * Created by pach on 18/10/17.
  */
object DestinationSpec  extends Properties("Destination") {

  import spinoco.protocol.email.SpecUtil._
  implicit val HeaderCodec = Destination.To.codec

  property("single-email") = protect {

    verify(
      "John Doe <john.doe@spinoco.com>"
      , Destination(DestinationType.To, EmailAddress("john.doe", "spinoco.com", Some("John Doe")), Nil)
    )

  }

  property("multiple-email") = protect {
    verify(
      "John Doe <john.doe@spinoco.com>, jannet.doe@spinoco.com"
      , Destination(DestinationType.To, EmailAddress("john.doe", "spinoco.com", Some("John Doe")), List(EmailAddress("jannet.doe", "spinoco.com", None)))
      , "John Doe <john.doe@spinoco.com>,\r\n jannet.doe@spinoco.com"
    )

  }

}
