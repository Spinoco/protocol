package spinoco.protocol.kafka.codec

import java.util.Date
import java.util.zip.CRC32

import scodec.{Attempt, Codec, Err}
import scodec.bits.ByteVector
import scodec.codecs._
import shapeless.{::, HNil}
import spinoco.protocol.kafka._
import spinoco.protocol.common.util._
import spinoco.protocol.kafka.Message.{CompressedMessages, SingleMessage}

/**
  *  Codec  for kafka's set of messages. Supports arbitrary nested compressed sets.
  */
object MessageSetCodec {

  /** Codec for set of messages expressed as vector of messages. Encodes both V0 and V1 format **/
  val  messageSetCodec:Codec[Vector[Message]] = {
    val entry:Codec[Message] =
      "Message Entry" | (
        ("Offset"       | int64 ) ~
          variableSizeBytes("Message Size"| int32 , impl.messageCodec)
      ).xmap(
        { case (offset, msg) => msg.updateOffset(offset) }
        , msg => (msg.offset,msg)
      )

    vector(entry)
  }

  object impl {
    val MagicByteV0:Byte = 0
    val MagicByteV1:Byte = 1

    //  3 bits holding information whether the message is or is not compressed
    //  yielding to None indicates message is not compressed
    val compressionAttribute:Codec[Option[Compression.Value]] = {
      def decode(compression:Int): Attempt[Option[Compression.Value]] = {
        if (compression == 0) Attempt.successful(None)
        else attempt(Compression(compression - 1)).map(Some(_))
      }
      def encode( compression : Option[Compression.Value]):Attempt[Int] =
        Attempt.successful(compression.map(_.id + 1).getOrElse(0))
      int(3).exmap(decode,encode)
    }

    val timeCodec:Codec[Option[Date]] = {
      int64.xmap(
        { t => if (t < 0) None else Some(new Date(t)) }
        , { _.map(_.getTime).getOrElse(-1l) }
      )
    }


    /** computes crc of kafka message **/
    def computeCrc(bytes:Array[Byte]):Int = {
      val crc = new CRC32()
      crc.update(bytes)
      (crc.getValue & 0xffffffffL).toInt
    }


    def decodeMessage(
      version:MessageVersion.Value
      , compression: Option[Compression.Value]
      , timeFlag: Boolean
      , time: Option[Date]
      , k: ByteVector
      , v: ByteVector
    ):Attempt[Message] = {
      val timeData = time.map {
        t => if (timeFlag) TimeData.LogAppendTime(t) else TimeData.CreateTime(t)
      }

      def decodeCompressed(compression: Compression.Value)(content:ByteVector):Attempt[Message] = {
        messageSetCodec.decode(content.bits).flatMap { result =>
          if (result.remainder.nonEmpty) Attempt.failure(Err(s"Nonepmty remainder when decoding compressed messgaes : ${result.remainder}"))
          else Attempt.successful(CompressedMessages(0, version, compression, timeData, result.value))
        }
      }

      compression match {
        case None => Attempt.successful(SingleMessage(0, version, timeData, k, v))

        case Some(compressionType) =>
          compressionType match {
            case Compression.GZIP => GZipCompression.inflate(v) flatMap decodeCompressed(Compression.GZIP)
            case Compression.Snappy => SnappyCompression.inflate(v) flatMap decodeCompressed(Compression.Snappy)
            case Compression.LZ4 => Attempt.failure(Err("LZ4 Compression not yet supported"))
          }
      }
    }

    def encodeMessage(msg:Message) = {

      def mkTime(timeStamp:Option[TimeData]):(Boolean, Option[Date]) = {
        val timeFlag = timeStamp.exists {
          case _:TimeData.LogAppendTime => true
          case _:TimeData.CreateTime => false
        }
        val time = timeStamp.map(_.time)
        timeFlag -> time
      }

      msg match {
        case sm: SingleMessage => attempt {
          val (timeFlag, time) = mkTime(sm.timeStamp)
           () :: timeFlag :: None :: time :: sm.key :: sm.value :: HNil
        }

        case cm: CompressedMessages =>
          def encodeCompressed(messages:Vector[Message]):Attempt[ByteVector] =
            messageSetCodec.encode(messages).map(_.bytes)

          val value =
            cm.compression match {
              case Compression.GZIP => encodeCompressed(cm.messages).flatMap(GZipCompression.deflate)
              case Compression.Snappy => encodeCompressed(cm.messages).flatMap(SnappyCompression.deflate)
              case Compression.LZ4 =>  Attempt.failure(Err("LZ4 Compression not yet supported"))
            }

          val (timeFlag, time) = mkTime(cm.timeStamp)

          value.map { vb =>
            () :: timeFlag :: Some(cm.compression) :: time :: ByteVector.empty :: vb :: HNil
          }

      }
    }

    val messageCodec:Codec[Message] = {
      "Message" | crcChecksum(
        ("MagicByte"          | byte).flatZip {
          case MagicByteV0 => v0Codec
          case MagicByteV1 => v1Codec
          case other => fail[Message](Err(s"Unexpected message magic: $other"))
        }.xmap(_._2,m => magicOf(m) -> m)
      )
    }


    def v0Codec:Codec[Message] = {

      "V0" | (
        ("Ignored Attribute"  | ignore(5)) ::
          ("Compression"        | compressionAttribute) ::
          ("Key"                | variableSizeBytes(int32, bytes) ) ::
          ("Value"              | variableSizeBytes(int32, bytes) )
        ).exmap(
        { case  _ :: compression ::  k :: v :: HNil =>
          decodeMessage(MessageVersion.V0,compression,false, None,k,v)
        }
        , (encodeMessage _).andThen( _.map {
          case  _ :: timeFlag :: compression :: time :: k :: v :: HNil =>
            () :: compression :: k :: v :: HNil
        })
      )

    }


    def v1Codec:Codec[Message] = {

      "V1" | (
        ("Ignored Attribute"  | ignore(4)) ::
        ("Time flag"          | bool) ::
        ("Compression"        | compressionAttribute) ::
        ("Time"               | timeCodec) ::
        ("Key"                | variableSizeBytes(int32, bytes) ) ::
        ("Value"              | variableSizeBytes(int32, bytes) )
      ).exmap(
        { case  _ :: timeFlag :: compression :: time :: k :: v :: HNil =>
          decodeMessage(MessageVersion.V1,compression,timeFlag, time,k,v)
        }
        , encodeMessage
      )

    }






    /**
      * codec that will write crc checksum before the encoded data by codec and when decoding
      * it verifies the crc will match before decoding reminder
      */
    def crcChecksum[A](codec:Codec[A]):Codec[A] = {
      def decode(crc32:Int,data:ByteVector):Attempt[A] = {
        val computedCrc32 = computeCrc(data.toArray)  //todo: avoid conversion to Array
        if (computedCrc32 != crc32) Attempt.failure(Err("CRC of message does not match"))
        else codec.decodeValue(data.bits)
      }

      def encode(a:A):Attempt[(Int,ByteVector)] = {
        codec.encode(a).map { bv =>
          val crc32 = computeCrc(bv.toByteArray) //todo avoid conversion to Array
          crc32 -> bv.bytes
        }
      }

      (("CRC32" | int32) ~ ("Data" | bytes) ).exmap(decode _ tupled,encode)

    }


    def magicOf(m:Message):Byte = {
      m match {
        case sm : Message.SingleMessage => sm.version.id.toByte
        case cm : Message.CompressedMessages => cm.version.id.toByte

      }
    }


  }


}
