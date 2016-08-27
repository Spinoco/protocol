package spinoco.protocol.kafka

import java.util.Date

import scodec.bits.ByteVector
import spinoco.protocol.kafka.Message.{CompressedMessages, SingleMessage}



object Message {

  /**
    * Single message to kafka
    * @param offset         Offset of this message. For producer this is ignored. If part of the
    *                       [[CompressedMessages]] then this is index of the message since the first message
    * @param timeStamp      Message timestamp. Producer sets this to None.
    * @param key            Key of the message. May be empty.
    * @param value          Value of the message on Right or MessageSet on left
    */
  case class SingleMessage(
     offset: Long
     , version: MessageVersion.Value
     , timeStamp: Option[TimeData]
     , key: ByteVector
     , value: ByteVector
   ) extends Message

  /**
    * More messages that are compressed over wire by specified compression codec
    * @param offset       Offset of the first message sent
    * @param compression  Compression Codec
    * @param timeStamp    timestamp of the first message
    * @param messages     Messages
    */
  case class CompressedMessages(
    offset: Long
    , version: MessageVersion.Value
    , compression: Compression.Value
    , timeStamp: Option[TimeData]
    , messages: Vector[Message]
  ) extends Message


}


sealed trait Message { self =>

  def offset:Long

  private[kafka] def updateOffset(newOffset:Long):Message = self match {
    case sm: SingleMessage => sm.copy(offset = newOffset)
    case cm: CompressedMessages => cm.copy(offset = newOffset)
  }

}


sealed trait TimeData {
  /** obtains time value**/
  def time:Date
}

object TimeData {
  case class LogAppendTime(time:Date) extends TimeData
  case class CreateTime(time:Date) extends TimeData
}


object Compression extends Enumeration {
  val GZIP, Snappy, LZ4 = Value
}

object MessageVersion extends Enumeration {
  val V0 = Value(0)
  val V1 = Value(1)
}