package spinoco.protocol.kafka.codec

import java.util
import java.util.{Date, Optional}

import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.network.ListenerName
import org.apache.kafka.common.protocol.Errors
import org.apache.kafka.common.record.{CompressionType, MemoryRecords, SimpleRecord, TimestampType}
import scodec.bits.ByteVector
import shapeless.tag
import shapeless.tag.@@
import spinoco.protocol.kafka.{MessageVersion, PartitionId, TimeData, TopicName}
import org.apache.kafka.common.requests._
import org.apache.kafka.common.security.auth.SecurityProtocol

import collection.JavaConverters._


/**
  * Licensed to the Apache Software Foundation (ASF) under one or more
  * contributor license agreements.  See the NOTICE file distributed with
  * this work for additional information regarding copyright ownership.
  * The ASF licenses this file to You under the Apache License, Version 2.0
  * (the "License"); you may not use this file except in compliance with
  * the License.  You may obtain a copy of the License at
  *
  *    http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */

object SerializationTestUtils {
  private val topic1 = "test1"
  private val topic2 = "test2"
  private val leader1 = 0
  private val isr1 = List(0, 1, 2)
  private val leader2 = 0
  private val isr2 = List(0, 2, 3)

  private val partitionDataFetchResponse0 = {
    import FetchResponse.PartitionData
    val data = MemoryRecords.withRecords(CompressionType.NONE, new SimpleRecord(0L, "first message".getBytes))
    val partitionData = new PartitionData[MemoryRecords](Errors.NONE, 0, 0, 0, List[FetchResponse.AbortedTransaction]().asJava, data)
    val dataMap = new util.LinkedHashMap[TopicPartition, PartitionData[MemoryRecords]]()
    dataMap.put(new TopicPartition(topic1, 0), partitionData)
    new FetchResponse[MemoryRecords](Errors.NONE, dataMap,0, 0)
  }
  private val partitionDataFetchResponse1 = {
    import FetchResponse.PartitionData
    val data = MemoryRecords.withRecords(CompressionType.NONE, new SimpleRecord(0L, "second message".getBytes))
    val partitionData = new PartitionData[MemoryRecords](Errors.NONE, 0, 0, 0, List[FetchResponse.AbortedTransaction]().asJava, data)
    val dataMap = new util.LinkedHashMap[TopicPartition, PartitionData[MemoryRecords]]()
    dataMap.put(new TopicPartition(topic1, 0), partitionData)
    new FetchResponse[MemoryRecords](Errors.NONE, dataMap,0, 0)
  }
  private val partitionDataFetchResponse2 = {
    import FetchResponse.PartitionData
    val data = MemoryRecords.withRecords(CompressionType.NONE, new SimpleRecord(0L, "third message".getBytes))
    val partitionData = new PartitionData[MemoryRecords](Errors.NONE, 0, 0, 0, List[FetchResponse.AbortedTransaction]().asJava, data)
    val dataMap = new util.LinkedHashMap[TopicPartition, PartitionData[MemoryRecords]]()
    dataMap.put(new TopicPartition(topic1, 0), partitionData)
    new FetchResponse[MemoryRecords](Errors.NONE, dataMap,0, 0)
  }
  private val partitionDataFetchResponse3 = {
    import FetchResponse.PartitionData
    val data = MemoryRecords.withRecords(CompressionType.NONE, new SimpleRecord(0L, "fourth message".getBytes))
    val partitionData = new PartitionData[MemoryRecords](Errors.NONE, 0, 0, 0, List[FetchResponse.AbortedTransaction]().asJava, data)
    val dataMap = new util.LinkedHashMap[TopicPartition, PartitionData[MemoryRecords]]()
    dataMap.put(new TopicPartition(topic1, 0), partitionData)
    new FetchResponse[MemoryRecords](Errors.NONE, dataMap,0, 0)
  }

  private val partitionDataFetchResponseMap = Map((0, partitionDataFetchResponse0), (1, partitionDataFetchResponse1), (2, partitionDataFetchResponse2), (3, partitionDataFetchResponse3))

  private val topicDataFetchResponse = {
    val groupedData = Array(topic1, topic2).flatMap(topic =>
      partitionDataFetchResponseMap.map(partitionAndData =>
        (new TopicPartition(topic, partitionAndData._1), partitionAndData._2)))
    groupedData.toSeq
  }

  private val partitionDataMessage0 = MemoryRecords.withRecords(CompressionType.NONE, new SimpleRecord(0L, "first message".getBytes))
  private val partitionDataMessage1 = MemoryRecords.withRecords(CompressionType.NONE, new SimpleRecord(0L, "second message".getBytes))
  private val partitionDataMessage2 = MemoryRecords.withRecords(CompressionType.NONE, new SimpleRecord(0L, "third message".getBytes))
  private val partitionDataMessage3 = MemoryRecords.withRecords(CompressionType.NONE, new SimpleRecord(0L, "fourth message".getBytes))
  private val partitionDataProducerRequestArray = Array(partitionDataMessage0, partitionDataMessage1, partitionDataMessage2, partitionDataMessage3)

  val topicDataProducerRequest = {
    val groupedData = Array(topic1, topic2).flatMap(topic =>
      partitionDataProducerRequestArray.zipWithIndex.map
      {
        case(partitionDataMessage, partition) =>
          (new TopicPartition(topic, partition), partitionDataMessage)
      })
    collection.mutable.Map(groupedData:_*)
  }

  private val requestInfos = Seq(
    new TopicPartition(topic1, 0) -> new FetchRequest.PartitionData(1000, 100, 100, Optional.empty()),
    new TopicPartition(topic1, 1) ->  new FetchRequest.PartitionData(2000, 100, 100, Optional.empty()),
    new TopicPartition(topic1, 2) ->  new FetchRequest.PartitionData(3000, 100, 100, Optional.empty()),
    new TopicPartition(topic1, 3) ->  new FetchRequest.PartitionData(4000, 100, 100, Optional.empty()),
    new TopicPartition(topic2, 0) ->  new FetchRequest.PartitionData(1000, 100, 100, Optional.empty()),
    new TopicPartition(topic2, 1) ->  new FetchRequest.PartitionData(2000, 100, 100, Optional.empty()),
    new TopicPartition(topic2, 2) ->  new FetchRequest.PartitionData(3000, 100, 100, Optional.empty()),
    new TopicPartition(topic2, 3) ->  new FetchRequest.PartitionData(4000, 100, 100, Optional.empty())
  )

  val listenerName =  ListenerName.forSecurityProtocol(SecurityProtocol.PLAINTEXT)

  def createTestProducerRequest: ProduceRequest = {
    ProduceRequest.Builder.forCurrentMagic(0, 0, topicDataProducerRequest.asJava).build(0)
  }

  def createTestProducerResponse: ProduceResponse = {
    new ProduceResponse(Map(
      new TopicPartition(topic1, 0) -> new ProduceResponse.PartitionResponse(Errors.NONE, 10001, 0, 0),
      new TopicPartition(topic2, 0) -> new ProduceResponse.PartitionResponse(Errors.NONE, 20001, 0, 0)
    ).asJava)
  }

  def createTestFetchRequest: FetchRequest = {
    val data = {
      val map = new util.LinkedHashMap[TopicPartition, FetchRequest.PartitionData]()
      requestInfos.foreach { case (topicPartition, data) => map.put(topicPartition, data)}
      map
    }
    FetchRequest.Builder.forConsumer(1000, 100, data).build(0)
  }

  def createTestOffsetFetchRequest: ListOffsetRequest = {
    ListOffsetRequest.Builder.forConsumer(false, IsolationLevel.READ_COMMITTED)
    .setTargetTimes(Map(new TopicPartition(topic1, 1) -> new ListOffsetRequest.PartitionData(-1L, Optional.empty[Integer]())).asJava)
    .build(0)
  }

  def createTestOffsetFetchResponse: ListOffsetResponse = {
    new ListOffsetResponse(Map(
      new TopicPartition(topic1, 1) -> new ListOffsetResponse.PartitionData(Errors.NONE, List[java.lang.Long](0L, 10L, 20L).asJava)
    ).asJava)
  }

  def kafka2SpinocoData(
    in: collection.mutable.Map[TopicPartition, MemoryRecords]
  ): Vector[(String @@ TopicName,Vector[(Int @@ PartitionId,Vector[spinoco.protocol.kafka.Message])])] = {
    in.toVector
    .map ({ case (tap,messages) =>
      val messageVector = {
        messages.records().asScala.toVector
      }

      val msgs = {
        messageVector.map { record =>
          val ts = {
            if (record.hasTimestampType(TimestampType.NO_TIMESTAMP_TYPE)) None
            else if (record.hasTimestampType(TimestampType.CREATE_TIME)) Some(TimeData.CreateTime(new Date(record.timestamp)))
            else if (record.hasTimestampType(TimestampType.LOG_APPEND_TIME)) Some(TimeData.LogAppendTime(new Date(record.timestamp)))
            else Some(TimeData.CreateTime(new Date(record.timestamp)))
          }

          val ma = {
            if (record.hasMagic(0)) MessageVersion.V0
            else MessageVersion.V1
          }

          if (record.isCompressed) throw new Exception("Compressed messages to yet supported")
          else spinoco.protocol.kafka.Message.SingleMessage(record.offset(), ma,ts,if (record.hasKey) ByteVector.view(record.key) else ByteVector.empty, ByteVector.view(record.value()))

        }
      }
      (tap.topic, tap.partition, msgs)
    })
    .groupBy { _._1 }
    .map { case (t,p) => t -> p.groupBy(_._2).toVector.sortBy(_._1).map { case (id, msgs) => tag[PartitionId](id) -> msgs.flatMap(_._3) }}
    .toVector.sortBy(_._1)
    .map { case (t, m) => tag[TopicName](t) -> m}


  }


}

