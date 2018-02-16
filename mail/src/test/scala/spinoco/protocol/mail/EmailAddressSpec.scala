package spinoco.protocol.mail

import org.scalacheck.Properties
import org.scalacheck.Prop._
import spinoco.protocol.mail.header.codec.EmailAddressCodec

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
    verify("John Doe <john.doe@spinoco.com>", EmailAddress("john.doe", "spinoco.com", Some("John Doe")), "\"John Doe\" <john.doe@spinoco.com>")
  }

  property("bracket-quoted-ascii-display") = protect {
    verify(
      "\"John Doe\" <john.doe@spinoco.com>"
      , EmailAddress("john.doe", "spinoco.com", Some("John Doe"))
      , "\"John Doe\" <john.doe@spinoco.com>"
    )
  }

  property("bracket-unicode-display") = protect {
    verify(
      "=?UTF-8?Q?Val=C3=A9rie_Doe?= <valerie.doe@spinoco.com>"
      , EmailAddress("valerie.doe", "spinoco.com", Some("Valérie Doe"))
      , "=?UTF-8?Q?Val=C3=A9rie_Doe?= <valerie.doe@spinoco.com>"
    )
  }

  property("bracket-unicode-display-base64-encoded") = protect {
    verify(
      "=?UTF-8?B?WiB0w6l0byBrYW5kaWTDoXRreSBzaSB2eWJlcmV0ZQ==?= <email.address@spinoco.com>"
      , EmailAddress("email.address", "spinoco.com", Some("Z této kandidátky si vyberete"))
      , "=?UTF-8?Q?Z_t=C3=A9to_kandid=C3=A1tky_si_vyberete?= <email.address@spinoco.com>"
    )
  }

  property("bracket-quoted-unicode-display") = protect {
    verify(
      "\"=?UTF-8?Q?Val=C3=A9rie_Doe?=\" <valerie.doe@spinoco.com>"
      , EmailAddress("valerie.doe", "spinoco.com", Some("Valérie Doe"))
      , "=?UTF-8?Q?Val=C3=A9rie_Doe?= <valerie.doe@spinoco.com>"
    )
  }

  property("bracket-quoted-unicode-display.contains.equals") = protect {
    verify(
      "\"=?UTF-8?Q?Val=C3=A9rie=3DDoe?=\" <valerie.doe@spinoco.com>"
      , EmailAddress("valerie.doe", "spinoco.com", Some("Valérie=Doe"))
      , "=?UTF-8?Q?Val=C3=A9rie=3DDoe?= <valerie.doe@spinoco.com>"
    )
  }

}
