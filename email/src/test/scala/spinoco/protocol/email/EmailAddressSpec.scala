package spinoco.protocol.email

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spinoco.protocol.email.header.codec.EmailAddressCodec

/**
  * Created by pach on 18/10/17.
  */
object EmailAddressSpec extends Properties("EmailAddress") {
  import SpecUtil._

  implicit val codec = EmailAddressCodec.codec

  property("plain-email") = protect {
    verify("john.doe@spinoco.com", EmailAddress("john.doe", "spinoco.com", None))
  }

  property ("bracket-only-email") = protect {
    verify("<john.doe@spinoco.com>", EmailAddress("john.doe", "spinoco.com", None), "john.doe@spinoco.com")
  }

  property("bracket-ascii-display") = protect {
    verify("John Doe <john.doe@spinoco.com>", EmailAddress("john.doe", "spinoco.com", Some("John Doe")))
  }

  property("bracket-quoted-ascii-display") = protect {
    verify(
      "\"John Doe\" <john.doe@spinoco.com>"
      , EmailAddress("john.doe", "spinoco.com", Some("John Doe"))
      , "John Doe <john.doe@spinoco.com>"
    )
  }

  property("bracket-unicode-display") = protect {
    verify(
      "=?UTF-8?Q?Val=C3=A9rie_Doe?= <valerie.doe@spinoco.com>"
      , EmailAddress("valerie.doe", "spinoco.com", Some("Valérie Doe"))
      , "=?utf-8?Q?Val=C3=A9rie_Doe?= <valerie.doe@spinoco.com>"
    )
  }

  property("bracket-unicode-display-base64-encoded") = protect {
    verify(
      "=?UTF-8?B?WiB0w6l0byBrYW5kaWTDoXRreSBzaSB2eWJlcmV0ZQ==?= <email.address@spinoco.com>"
      , EmailAddress("email.address", "spinoco.com", Some("Z této kandidátky si vyberete"))
      , "=?utf-8?Q?Z_t=C3=A9to_kandid=C3=A1tky_si_vyberete?= <email.address@spinoco.com>"
    )
  }


}
