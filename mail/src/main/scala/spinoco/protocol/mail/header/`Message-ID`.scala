package spinoco.protocol.mail.header

import scodec.Codec
import shapeless.tag.@@
import spinoco.protocol.mail.header.codec._

/**
  * RFC 5322 3.6.4
  *
  * The "Message-ID:" field provides a unique message identifier that
  * refers to a particular version of a particular message.  The
  * uniqueness of the message identifier is guaranteed by the host that
  * generates it (see below).  This message identifier is intended to be
  * machine readable and not necessarily meaningful to humans.  A message
  * identifier pertains to exactly one version of a particular message;
  * subsequent revisions to the message each receive new message
  * identifiers.
  *
  */
case class `Message-ID`(id: String @@ `Message-ID`) extends DefaultEmailHeaderField


object `Message-ID` extends DefaultHeaderDescription[`Message-ID`] {

  val codec: Codec[`Message-ID`] =
    msgIdCodec.xmap(`Message-ID`.apply, _.id)

}