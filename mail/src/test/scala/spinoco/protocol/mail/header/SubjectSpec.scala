package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties


object SubjectSpec extends Properties("Subject") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = Subject.codec


  property("simple") = protect {

    verify(
      "A Plain subject"
      , Subject("A Plain subject")
    )

  }

  property("encoded.unicode.Q") = protect {

    verify(
      "=?utf-8?Q?Petr_Poled=C5=88=C3=A1k?="
      , Subject("Petr Poledňák")
      , "=?UTF-8?Q?Petr_Poled=C5=88=C3=A1k?="
    )

  }


  property("encoded.unicode.B") = protect {

    verify(
      "=?utf-8?B?UMWZZWQgdMSbbWlobGUga2xvYm91ayBkb2zFryE=?="
      , Subject("Před těmihle klobouk dolů!")
      ,"=?UTF-8?Q?P=C5=99ed_t=C4=9Bmihle_klobouk_dol=C5=AF!?="
    )

  }

  property("encoded.unicode.multiple-chars.B") = protect {
    verify(
      "=?utf-8?b?8J+QnyBSeWLDrSBzcGVjacOhbA==?="
      , Subject("🐟 Rybí speciál")
      ,"=?UTF-8?Q?=F0=9F=90=9F_Ryb=C3=AD_speci=C3=A1l?="
    )
  }

  property("encoded.unicode.multiple-chars.Q") = protect {
    verify(
      "=?UTF-8?Q?=F0=9F=90=9F_Ryb=C3=AD_speci=C3=A1l?="
      , Subject("🐟 Rybí speciál")
      ,"=?UTF-8?Q?=F0=9F=90=9F_Ryb=C3=AD_speci=C3=A1l?="
    )
  }

  property("encoded.unicode.Q.cfws") = protect {

    verify(
      "A Plain subject"
      , Subject("A Plain subject")
    )

  }

  property("encode.unicode.Q.multiple-words") = protect {
    verify(
      "=?utf-8?Q?Petr_Poled=C5=88=C3=A1k?= =?utf-8?Q?_Petr_Poled=C5=88=C3=A1k?=\r\n =?utf-8?Q?_Petr_Poled=C5=88=C3=A1k_123456789012345678901234567890?="
      , Subject("Petr Poledňák Petr Poledňák Petr Poledňák 123456789012345678901234567890")
      , "=?UTF-8?Q?Petr_Poled=C5=88=C3=A1k_Petr_Poled=C5=88=C3=A1k_Petr_Poled=C5=88?=\r\n =?UTF-8?Q?k_123456789012345678901234567890?="
    )
  }


}
