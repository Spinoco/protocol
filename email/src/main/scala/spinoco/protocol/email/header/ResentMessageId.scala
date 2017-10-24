package spinoco.protocol.email.header

import scodec.Codec
import shapeless.tag.@@
import spinoco.protocol.email.header.codec.msgIdCodec

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
case class ResentMessageId(id: String @@ MessageId) extends EmailHeaderField {
  def name: String = ResentMessageId.name
}


object ResentMessageId extends HeaderDescription[ResentMessageId] {

  val name: String = "Resent-Message-ID"

  val codec: Codec[ResentMessageId] =
    msgIdCodec.xmap(ResentMessageId.apply, _.id)


  def fieldCodec: Codec[EmailHeaderField] = codec.upcast

}
