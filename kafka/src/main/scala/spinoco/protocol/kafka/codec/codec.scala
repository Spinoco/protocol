package spinoco.protocol
package kafka

import java.util.Date

import scodec.{Attempt, Codec, Err}
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.tag
import shapeless.tag._
import spinoco.protocol.common.util._


package object codec {

  /** kafka string that may signal None (null) by passing -1 to its size **/
  val kafkaOptionalString: Codec[Option[String]] = {
    def encode(str:Option[String]):Attempt[(Int, ByteVector)] = {
      str match {
        case None => Attempt.successful((-1, ByteVector.empty))
        case Some(s) => Attempt.fromEither(
          ByteVector.encodeUtf8(s)
          .left.map(ex => Err(ex.getMessage))
          .map(bv => (bv.size.toInt,bv))
        )
      }
    }
    def decode(sz:Int, data:ByteVector):Attempt[Option[String]] = {
      if (sz < 0) Attempt.Successful(None)
      else Attempt.fromEither{
        data.decodeUtf8
        .left.map(ex => Err(ex.getMessage))
        .map(s => Some(s))
      }
    }

    int16.flatZip { sz =>  bytes(sz max 0) }
    .exmap(decode _ tupled, encode)
  }

  /** kafka string that must be present **/
  val kafkaRequiredString:Codec[String] = {
    def encode(s:String):Attempt[Option[String]] = Attempt.Successful(Some(s))
    def decode(ms:Option[String]):Attempt[String] = ms match {
      case None => Attempt.failure(Err("Required String but got None (null)"))
      case Some(s) => Attempt.successful(s)
    }
    kafkaOptionalString.exmap(decode,encode)
  }

  /** encoded kafka array **/
  def kafkaArray[A](codec:Codec[A]):Codec[Vector[A]] =
    vectorOfN(int32,codec)


  val kafkaError:Codec[Option[ErrorType.Value]] =
    int16.exmap(
      {
        case 0 => Attempt.successful(None)
        case code => attempt(Some(ErrorType(code)))
      }
      , e => Attempt.successful(e.map(_.id).getOrElse(0))
    )

  val kafkaTopicName:Codec[String @@ TopicName] =
    kafkaRequiredString.xmap(tag[TopicName](_), s => s:String)

  val kafkaPartitionId:Codec[Int @@ PartitionId] =
    int32.xmap(tag[PartitionId](_), i => i:Int)

  val kafkaOffset:Codec[Long @@ Offset] =
    int64.xmap(tag[Offset](_), l => l:Long)

  val kafkaBrokerId:Codec[Int @@ Broker] =
    int32.xmap(tag[Broker](_), i => i:Int)

  val kafkaDate: Codec[Date]  =
    int64.xmap(new Date(_), _.getTime)


}
