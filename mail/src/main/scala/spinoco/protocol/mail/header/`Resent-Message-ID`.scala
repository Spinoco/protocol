package spinoco.protocol.mail.header

import scodec.Codec
import shapeless.tag.@@
import spinoco.protocol.mail.header.codec.msgIdCodec

/**
  * RFC 5322 3.6.6:
  *
  * Resent fields SHOULD be added to any message that is reintroduced by
  * a user into the transport system.  A separate set of resent fields
  * SHOULD be added each time this is done.
  *
  * For instance, the "Resent-Message-ID:" field corresponds to
  * the "Message-ID:" field
  *
  */
case class `Resent-Message-ID`(id: String @@ `Message-ID`) extends DefaultEmailHeaderField


object `Resent-Message-ID` extends DefaultHeaderDescription[`Resent-Message-ID`] {

  val codec: Codec[`Resent-Message-ID`] =
    msgIdCodec.xmap(`Resent-Message-ID`.apply, _.id)


}
