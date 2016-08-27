package spinoco.protocol.kafka.codec


import java.util.Date

import kafka.message.{ByteBufferMessageSet, GZIPCompressionCodec, MessageAndOffset, SnappyCompressionCodec, Message => KMessage}
import spinoco.protocol.common.ProtocolSpec
import scodec.{Attempt, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.kafka.Message.{CompressedMessages, SingleMessage}
import spinoco.protocol.kafka.{Compression, MessageVersion}
import spinoco.protocol.kafka.TimeData.CreateTime


class MessageSetCodecSpec extends ProtocolSpec {
  val magic0:Byte = 0
  val magic1:Byte = 1

  val now = new Date()

  val kMessages0 = Seq(
    new KMessage(ByteVector(1,2,3).toArray, KMessage.NoTimestamp, magic0)
    , new KMessage(ByteVector(4,5,6).toArray, ByteVector(0,1,2).toArray, KMessage.NoTimestamp, magic0)
    , new KMessage(ByteVector(7,8,9).toArray, ByteVector(3,4,5).toArray, KMessage.NoTimestamp, magic0)
  )

  val kMessages1 = Seq(
    new KMessage(ByteVector(1,2,3).toArray, KMessage.NoTimestamp, magic1)
    , new KMessage(ByteVector(4,5,6).toArray, ByteVector(0,1,2).toArray, KMessage.NoTimestamp, magic1)
    , new KMessage(ByteVector(7,8,9).toArray, ByteVector(3,4,5).toArray, now.getTime, magic1)
  )

  val sMessages0 = kMessages0.zipWithIndex.map { case (km,idx) => kafka2Spinoco(km,idx) }.toVector
  val sMessages1 = kMessages1.zipWithIndex.map { case (km,idx) => kafka2Spinoco(km,idx) }.toVector




  "MessageSet" - {
    "deserializes V0" - {
      "when messages are not compressed" in {
        val buff = new ByteBufferMessageSet(kMessages0: _*)
        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.getBuffer).bits)

        r shouldBe Attempt.successful(
          DecodeResult(
            sMessages0
            , BitVector.empty
          )
        )
      }

      "when messages are compressed (GZIP)" in {
        val buff = new ByteBufferMessageSet(GZIPCompressionCodec, kMessages0: _*)

        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.getBuffer).bits)

        r shouldBe Attempt.successful {
          DecodeResult(
            Vector(
              CompressedMessages(
                offset = 2
                , version = MessageVersion.V0
                , compression = Compression.GZIP
                , timeStamp = None
                , messages = sMessages0
              )
            )
            , BitVector.empty
          )
        }

      }

      "when messages are compressed (Snappy)" in {
        val buff = new ByteBufferMessageSet(SnappyCompressionCodec, kMessages0: _*)

        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.getBuffer).bits)

        r shouldBe Attempt.successful {
          DecodeResult(
            Vector(
              CompressedMessages(
                offset = 2
                , version = MessageVersion.V0
                , compression = Compression.Snappy
                , timeStamp = None
                , messages = sMessages0
              )
            )
            , BitVector.empty
          )
        }

      }

    }

    "deserializes V1" - {
      "when messages are not compressed" in {
        val buff = new ByteBufferMessageSet(kMessages1: _*)
        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.getBuffer).bits)

        r shouldBe Attempt.successful(
          DecodeResult(
            sMessages1
            , BitVector.empty
          )
        )
      }

      "when messages are compressed (GZIP)" in {
        val buff = new ByteBufferMessageSet(GZIPCompressionCodec, kMessages1: _*)

        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.getBuffer).bits)

        r shouldBe Attempt.successful {
          DecodeResult(
            Vector(
              CompressedMessages(
                offset = 2
                , version = MessageVersion.V1
                , compression = Compression.GZIP
                , timeStamp = Some(CreateTime(now))
                , messages = sMessages1
              )
            )
            , BitVector.empty
          )
        }

      }

      "when messages are compressed (Snappy)" in {
        val buff = new ByteBufferMessageSet(SnappyCompressionCodec, kMessages1: _*)

        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.getBuffer).bits)

        r shouldBe Attempt.successful {
          DecodeResult(
            Vector(
              CompressedMessages(
                offset = 2
                , version = MessageVersion.V1
                , compression = Compression.Snappy
                , timeStamp = Some(CreateTime(now))
                , messages = sMessages1
              )
            )
            , BitVector.empty
          )
        }

      }

    }

    "serializes V0" - {

      "when messages are not compressed" in {
        val encoded = MessageSetCodec.messageSetCodec.encode(sMessages0)
        val kafkaSet = new ByteBufferMessageSet(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.toVector.map { case MessageAndOffset(m,o) => kafka2Spinoco(m,o) } shouldBe sMessages0
      }

      "when messages are compressed (GZIP)" in {
        val compressed = CompressedMessages(10,MessageVersion.V0,Compression.GZIP, None, sMessages0)
        val encoded = MessageSetCodec.messageSetCodec.encode(Vector(compressed))
        val kafkaSet = new ByteBufferMessageSet(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.iterator.toVector.map { case MessageAndOffset(m,o) => kafka2Spinoco(m,o) } shouldBe sMessages0
      }


      "when messages are compressed (Snappy)" in {
        val compressed = CompressedMessages(10,MessageVersion.V0,Compression.Snappy, None, sMessages0)
        val encoded = MessageSetCodec.messageSetCodec.encode(Vector(compressed))
        val kafkaSet = new ByteBufferMessageSet(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.iterator.toVector.map { case MessageAndOffset(m,o) => kafka2Spinoco(m,o) } shouldBe sMessages0
      }


    }


    "serializes V1" - {

      "when messages are not compressed" in {
        val encoded = MessageSetCodec.messageSetCodec.encode(sMessages1)
        val kafkaSet = new ByteBufferMessageSet(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.toVector.map { case MessageAndOffset(m,o) => kafka2Spinoco(m,o) } shouldBe sMessages1
      }

      "when messages are compressed (GZIP)" in {
        val compressed = CompressedMessages(10,MessageVersion.V1,Compression.GZIP, Some(CreateTime(now)), sMessages1)
        val encoded = MessageSetCodec.messageSetCodec.encode(Vector(compressed))
        val kafkaSet = new ByteBufferMessageSet(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.iterator.toVector.map { case MessageAndOffset(m,o) => kafka2Spinoco(m,o-8) } shouldBe sMessages1
      }


      "when messages are compressed (Snappy)" in {
        val compressed = CompressedMessages(10,MessageVersion.V1,Compression.Snappy, Some(CreateTime(now)), sMessages1)
        val encoded = MessageSetCodec.messageSetCodec.encode(Vector(compressed))
        val kafkaSet = new ByteBufferMessageSet(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.iterator.toVector.map { case MessageAndOffset(m,o) => kafka2Spinoco(m,o-8) } shouldBe sMessages1
      }


    }


  }


  def kafka2Spinoco(km:KMessage, idx:Long):SingleMessage = {
    val ts = km.timestamp match {
      case KMessage.NoTimestamp => None
      case value => Some(CreateTime(new Date(value)))
    }
    val k = if (km.key != null) ByteVector.view(km.key) else ByteVector.empty
    SingleMessage(idx, MessageVersion(km.magic), ts, k, ByteVector.view(km.payload))
  }





}
