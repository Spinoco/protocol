package spinoco.protocol.kafka.codec


import java.util.Date

import org.apache.kafka.common.record.{CompressionType, MemoryRecords, Record, RecordBatch, SimpleRecord}
import spinoco.protocol.common.ProtocolSpec
import scodec.{Attempt, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.kafka.Message.{CompressedMessages, SingleMessage}
import spinoco.protocol.kafka.{Compression, MessageVersion}
import spinoco.protocol.kafka.TimeData.CreateTime
import collection.JavaConverters._


class MessageSetCodecSpec extends ProtocolSpec {
  val magic0:Byte = 0
  val magic1:Byte = 1

  val now = new Date()

  val kMessages0 = Seq(
    new SimpleRecord(ByteVector(1,2,3).toArray)
    , new SimpleRecord(ByteVector(4,5,6).toArray, ByteVector(0,1,2).toArray)
    , new SimpleRecord(ByteVector(7,8,9).toArray, ByteVector(3,4,5).toArray)
  )

  val kMessages1 = Seq(
    new SimpleRecord(ByteVector(1,2,3).toArray)
    , new SimpleRecord(ByteVector(4,5,6).toArray, ByteVector(0,1,2).toArray)
    , new SimpleRecord(now.getTime, ByteVector(7,8,9).toArray, ByteVector(3,4,5).toArray)
  )

  val sMessages0 = kMessages0.zipWithIndex.map { case (km,idx) => kafka2SpinocoSR(km,idx, MessageVersion.V0) }.toVector
  val sMessages1 = kMessages1.zipWithIndex.map { case (km,idx) => kafka2SpinocoSR(km,idx, MessageVersion.V1) }.toVector




  "MessageSet" - {
    "deserializes V0" - {
      "when messages are not compressed" in {
        val buff = MemoryRecords.withRecords(magic0, CompressionType.NONE, kMessages0: _*)
        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.buffer()).bits)

        r shouldBe Attempt.successful(
          DecodeResult(
            sMessages0
            , BitVector.empty
          )
        )
      }

      "when messages are compressed (GZIP)" in {
        val buff = MemoryRecords.withRecords(magic0, CompressionType.GZIP, kMessages0: _*)

        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.buffer()).bits)

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
        val buff = MemoryRecords.withRecords(magic0, CompressionType.SNAPPY, kMessages0: _*)
        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.buffer()).bits)

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
        val buff = MemoryRecords.withRecords(magic1, CompressionType.NONE, kMessages1: _*)
        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.buffer()).bits)

        r shouldBe Attempt.successful(
          DecodeResult(
            sMessages1
            , BitVector.empty
          )
        )
      }

      "when messages are compressed (GZIP)" in {
        val buff = MemoryRecords.withRecords(magic1, CompressionType.GZIP, kMessages1: _*)
        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.buffer()).bits)


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
        val buff = MemoryRecords.withRecords(magic1, CompressionType.SNAPPY, kMessages1: _*)
        val r = MessageSetCodec.messageSetCodec.decode(ByteVector(buff.buffer()).bits)


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
        val kafkaSet = MemoryRecords.readableRecords(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.records().asScala.toVector.map { record => kafka2Spinoco(record) } shouldBe sMessages0
      }

      "when messages are compressed (GZIP)" in {
        val compressed = CompressedMessages(10,MessageVersion.V0,Compression.GZIP, None, sMessages0)
        val encoded = MessageSetCodec.messageSetCodec.encode(Vector(compressed))
        val kafkaSet = MemoryRecords.readableRecords(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.records().asScala.toVector.map { record => kafka2Spinoco(record) } shouldBe sMessages0
      }


      "when messages are compressed (Snappy)" in {
        val compressed = CompressedMessages(10,MessageVersion.V0,Compression.Snappy, None, sMessages0)
        val encoded = MessageSetCodec.messageSetCodec.encode(Vector(compressed))
        val kafkaSet = MemoryRecords.readableRecords(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.records().asScala.toVector.map { record => kafka2Spinoco(record) } shouldBe sMessages0
      }


    }


    "serializes V1" - {

      "when messages are not compressed" in {
        val encoded = MessageSetCodec.messageSetCodec.encode(sMessages1)
        val kafkaSet = MemoryRecords.readableRecords(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.records().asScala.toVector.map { record => kafka2Spinoco(record) } shouldBe sMessages1
      }

      "when messages are compressed (GZIP)" in {
        val compressed = CompressedMessages(10,MessageVersion.V1,Compression.GZIP, Some(CreateTime(now)), sMessages1)
        val encoded = MessageSetCodec.messageSetCodec.encode(Vector(compressed))
        val kafkaSet = MemoryRecords.readableRecords(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.records().asScala.toVector.map { record => kafka2Spinoco(record, -8) } shouldBe sMessages1
      }


      "when messages are compressed (Snappy)" in {
        val compressed = CompressedMessages(10,MessageVersion.V1,Compression.Snappy, Some(CreateTime(now)), sMessages1)
        val encoded = MessageSetCodec.messageSetCodec.encode(Vector(compressed))
        val kafkaSet = MemoryRecords.readableRecords(encoded.map(_.toByteBuffer).getOrElse(fail("Failed to encode")))

        kafkaSet.records().asScala.toVector.map { record => kafka2Spinoco(record, -8) } shouldBe sMessages1
      }
    }
  }


  def kafka2SpinocoSR(km: SimpleRecord, idx: Long, magic: MessageVersion.Value): SingleMessage = {
    val ts = {
      if (km.timestamp() == RecordBatch.NO_TIMESTAMP) None
      else Some(CreateTime(new Date(km.timestamp)))
    }

    val k = if (km.key != null) ByteVector.view(km.key) else ByteVector.empty
    SingleMessage(idx, magic, ts, k, ByteVector.view(km.value()))
  }

  def kafka2Spinoco(km: Record, changeOffsetBy: Int = 0): SingleMessage = {
    val ts = {
      if (km.timestamp() == RecordBatch.NO_TIMESTAMP) None
      else Some(CreateTime(new Date(km.timestamp)))
    }

    val magic = {
      if (km.hasMagic(magic0)) MessageVersion.V0
      else MessageVersion.V1
    }

    val k = if (km.key != null) ByteVector.view(km.key) else ByteVector.empty
    SingleMessage(km.offset() + changeOffsetBy, magic, ts, k, ByteVector.view(km.value()))
  }




}
