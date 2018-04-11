package spinoco.protocol.mail.header.codec

import org.scalacheck.Prop._
import org.scalacheck.Properties

object RFC2047CodecSpec extends Properties("RFC2047Codec") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val codec = RFC2047Codec.quotedCodec

  property("decode.quoted.text") = protect {
    verify(
      "\"=?utf-8?Q?Petr_Poled=C5=88=C3=A1?= =?utf-8?Q?_Petr_Poled=C5=88=C3=A1?=\""
      , "Petr Poledňá Petr Poledňá"
      , "\"=?UTF-8?Q?Petr_Poled=C5=88=C3=A1_Petr_Poled=C5=88=C3=A1?=\""
    )
  }

}
