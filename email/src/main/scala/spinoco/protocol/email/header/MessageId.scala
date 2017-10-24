package spinoco.protocol.email.header

import scodec.Codec
import shapeless.tag.@@
import spinoco.protocol.email.header.codec._

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
case class MessageId(id: String @@ MessageId) extends EmailHeaderField {
  def name: String = MessageId.name
}


object MessageId extends HeaderDescription[MessageId] {

  val name: String = "Message-ID"

  val codec: Codec[MessageId] =
    msgIdCodec.xmap(MessageId.apply, _.id)

  def fieldCodec: Codec[EmailHeaderField] = codec.upcast


}