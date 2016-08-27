package spinoco.protocol.kafka.codec

import java.util.Date

import kafka.api._
import kafka.cluster.{Broker, EndPoint}
import kafka.common.{OffsetAndMetadata, OffsetMetadata, OffsetMetadataAndError, TopicAndPartition}
import kafka.controller.LeaderIsrAndControllerEpoch
import kafka.message._
import kafka.utils.SystemTime
import org.apache.kafka.common.protocol.{Errors, SecurityProtocol}
import org.apache.kafka.common.record.TimestampType
import scodec.bits.ByteVector
import shapeless.tag
import shapeless.tag.@@
import spinoco.protocol.kafka.{Compression, PartitionId, TimeData, TopicName, MessageVersion}

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
  private val partitionDataFetchResponse0 = new FetchResponsePartitionData(messages = new ByteBufferMessageSet(new Message("first message".getBytes)))
  private val partitionDataFetchResponse1 = new FetchResponsePartitionData(messages = new ByteBufferMessageSet(new Message("second message".getBytes)))
  private val partitionDataFetchResponse2 = new FetchResponsePartitionData(messages = new ByteBufferMessageSet(new Message("third message".getBytes)))
  private val partitionDataFetchResponse3 = new FetchResponsePartitionData(messages = new ByteBufferMessageSet(new Message("fourth message".getBytes)))
  private val partitionDataFetchResponseMap = Map((0, partitionDataFetchResponse0), (1, partitionDataFetchResponse1), (2, partitionDataFetchResponse2), (3, partitionDataFetchResponse3))

  private val topicDataFetchResponse = {
    val groupedData = Array(topic1, topic2).flatMap(topic =>
      partitionDataFetchResponseMap.map(partitionAndData =>
        (TopicAndPartition(topic, partitionAndData._1), partitionAndData._2)))
    collection.immutable.Map(groupedData:_*)
  }

  private val partitionDataMessage0 = new ByteBufferMessageSet(new Message("first message".getBytes))
  private val partitionDataMessage1 = new ByteBufferMessageSet(new Message("second message".getBytes))
  private val partitionDataMessage2 = new ByteBufferMessageSet(new Message("third message".getBytes))
  private val partitionDataMessage3 = new ByteBufferMessageSet(new Message("fourth message".getBytes))
  private val partitionDataProducerRequestArray = Array(partitionDataMessage0, partitionDataMessage1, partitionDataMessage2, partitionDataMessage3)

  val topicDataProducerRequest = {
    val groupedData = Array(topic1, topic2).flatMap(topic =>
      partitionDataProducerRequestArray.zipWithIndex.map
      {
        case(partitionDataMessage, partition) =>
          (TopicAndPartition(topic, partition), partitionDataMessage)
      })
    collection.mutable.Map(groupedData:_*)
  }

  private val requestInfos = collection.immutable.Map(
    TopicAndPartition(topic1, 0) -> PartitionFetchInfo(1000, 100),
    TopicAndPartition(topic1, 1) -> PartitionFetchInfo(2000, 100),
    TopicAndPartition(topic1, 2) -> PartitionFetchInfo(3000, 100),
    TopicAndPartition(topic1, 3) -> PartitionFetchInfo(4000, 100),
    TopicAndPartition(topic2, 0) -> PartitionFetchInfo(1000, 100),
    TopicAndPartition(topic2, 1) -> PartitionFetchInfo(2000, 100),
    TopicAndPartition(topic2, 2) -> PartitionFetchInfo(3000, 100),
    TopicAndPartition(topic2, 3) -> PartitionFetchInfo(4000, 100)
  )

  private val brokers = List(new Broker(0, Map(SecurityProtocol.PLAINTEXT -> EndPoint("localhost", 1011, SecurityProtocol.PLAINTEXT))),
    new Broker(1, Map(SecurityProtocol.PLAINTEXT -> EndPoint("localhost", 1012, SecurityProtocol.PLAINTEXT))),
    new Broker(2, Map(SecurityProtocol.PLAINTEXT -> EndPoint("localhost", 1013, SecurityProtocol.PLAINTEXT))))
  private val brokerEndpoints = brokers.map(_.getBrokerEndPoint(SecurityProtocol.PLAINTEXT))

  private val partitionMetaData0 = new PartitionMetadata(0, Some(brokerEndpoints.head), replicas = brokerEndpoints, isr = brokerEndpoints, errorCode = 0)
  private val partitionMetaData1 = new PartitionMetadata(1, Some(brokerEndpoints.head), replicas = brokerEndpoints, isr = brokerEndpoints.tail, errorCode = 1)
  private val partitionMetaData2 = new PartitionMetadata(2, Some(brokerEndpoints.head), replicas = brokerEndpoints, isr = brokerEndpoints, errorCode = 2)
  private val partitionMetaData3 = new PartitionMetadata(3, Some(brokerEndpoints.head), replicas = brokerEndpoints, isr = brokerEndpoints.tail.tail, errorCode = 3)
  private val partitionMetaDataSeq = Seq(partitionMetaData0, partitionMetaData1, partitionMetaData2, partitionMetaData3)
  private val topicmetaData1 = new TopicMetadata(topic1, partitionMetaDataSeq)
  private val topicmetaData2 = new TopicMetadata(topic2, partitionMetaDataSeq)

  private val leaderAndIsr0 = new LeaderAndIsr(leader = brokers.head.id, isr = brokers.map(_.id))
  private val leaderAndIsr1 = new LeaderAndIsr(leader = brokers.head.id, isr = brokers.tail.map(_.id))
  private val leaderAndIsr2 = new LeaderAndIsr(leader = brokers.head.id, isr = brokers.map(_.id))
  private val leaderAndIsr3 = new LeaderAndIsr(leader = brokers.head.id, isr = brokers.tail.map(_.id))

  private val leaderIsrAndControllerEpoch0 = new LeaderIsrAndControllerEpoch(leaderAndIsr0, controllerEpoch = 0)
  private val leaderIsrAndControllerEpoch1 = new LeaderIsrAndControllerEpoch(leaderAndIsr1, controllerEpoch = 0)
  private val leaderIsrAndControllerEpoch2 = new LeaderIsrAndControllerEpoch(leaderAndIsr2, controllerEpoch = 0)
  private val leaderIsrAndControllerEpoch3 = new LeaderIsrAndControllerEpoch(leaderAndIsr3, controllerEpoch = 0)

  private val partitionStateInfo0 = new PartitionStateInfo(leaderIsrAndControllerEpoch0, brokers.map(_.id).toSet)
  private val partitionStateInfo1 = new PartitionStateInfo(leaderIsrAndControllerEpoch1, brokers.map(_.id).toSet)
  private val partitionStateInfo2 = new PartitionStateInfo(leaderIsrAndControllerEpoch2, brokers.map(_.id).toSet)
  private val partitionStateInfo3 = new PartitionStateInfo(leaderIsrAndControllerEpoch3, brokers.map(_.id).toSet)

  private val updateMetadataRequestPartitionStateInfo = collection.immutable.Map(
    TopicAndPartition(topic1,0) -> partitionStateInfo0,
    TopicAndPartition(topic1,1) -> partitionStateInfo1,
    TopicAndPartition(topic1,2) -> partitionStateInfo2,
    TopicAndPartition(topic1,3) -> partitionStateInfo3
  )

  def createTestProducerRequest: ProducerRequest = {
    new ProducerRequest(1, "client 1", 0, 1000, topicDataProducerRequest)
  }

  def createTestProducerResponse: ProducerResponse =
    ProducerResponse(1, Map(
      TopicAndPartition(topic1, 0) -> ProducerResponseStatus(0.toShort, 10001),
      TopicAndPartition(topic2, 0) -> ProducerResponseStatus(0.toShort, 20001)
    ), ProducerRequest.CurrentVersion, 100)

  def createTestFetchRequest: FetchRequest = {
    new FetchRequest(requestInfo = requestInfos)
  }

  def createTestFetchResponse: FetchResponse = {
    FetchResponse(1, topicDataFetchResponse)
  }

  def createTestOffsetRequest = new OffsetRequest(
    collection.immutable.Map(TopicAndPartition(topic1, 1) -> PartitionOffsetRequestInfo(1000, 200)),
    replicaId = 0
  )

  def createTestOffsetResponse: OffsetResponse = {
    new OffsetResponse(0, collection.immutable.Map(
      TopicAndPartition(topic1, 1) -> PartitionOffsetsResponse(Errors.NONE.code, Seq(1000l, 2000l, 3000l, 4000l)))
    )
  }

  def createTestOffsetCommitRequestV2: OffsetCommitRequest = {
    new OffsetCommitRequest(
      groupId = "group 1",
      retentionMs = SystemTime.milliseconds,
      requestInfo=collection.immutable.Map(
        TopicAndPartition(topic1, 0) -> OffsetAndMetadata(42L, "some metadata"),
        TopicAndPartition(topic1, 1) -> OffsetAndMetadata(100L, OffsetMetadata.NoMetadata)
      ))
  }

  def createTestOffsetCommitRequestV1: OffsetCommitRequest = {
    new OffsetCommitRequest(
      versionId = 1,
      groupId = "group 1",
      requestInfo = collection.immutable.Map(
        TopicAndPartition(topic1, 0) -> OffsetAndMetadata(42L, "some metadata", SystemTime.milliseconds),
        TopicAndPartition(topic1, 1) -> OffsetAndMetadata(100L, OffsetMetadata.NoMetadata, SystemTime.milliseconds)
      ))
  }

  def createTestOffsetCommitRequestV0: OffsetCommitRequest = {
    new OffsetCommitRequest(
      versionId = 0,
      groupId = "group 1",
      requestInfo = collection.immutable.Map(
        TopicAndPartition(topic1, 0) -> OffsetAndMetadata(42L, "some metadata"),
        TopicAndPartition(topic1, 1) -> OffsetAndMetadata(100L, OffsetMetadata.NoMetadata)
      ))
  }

  def createTestOffsetCommitResponse: OffsetCommitResponse = {
    new OffsetCommitResponse(collection.immutable.Map(TopicAndPartition(topic1, 0) -> Errors.NONE.code,
      TopicAndPartition(topic1, 1) -> Errors.NONE.code))
  }

  def createTestOffsetFetchRequest: OffsetFetchRequest = {
    new OffsetFetchRequest("group 1", Seq(
      TopicAndPartition(topic1, 0),
      TopicAndPartition(topic1, 1)
    ))
  }

  def createTestOffsetFetchResponse: OffsetFetchResponse = {
    new OffsetFetchResponse(collection.immutable.Map(
      TopicAndPartition(topic1, 0) -> OffsetMetadataAndError(42L, "some metadata", Errors.NONE.code),
      TopicAndPartition(topic1, 1) -> OffsetMetadataAndError(100L, OffsetMetadata.NoMetadata, Errors.UNKNOWN_TOPIC_OR_PARTITION.code)
    ))
  }

  def createConsumerMetadataRequest: GroupCoordinatorRequest = {
    GroupCoordinatorRequest("group 1", clientId = "client 1")
  }

  def createConsumerMetadataResponse: GroupCoordinatorResponse = {
    GroupCoordinatorResponse(Some(brokers.head.getBrokerEndPoint(SecurityProtocol.PLAINTEXT)), Errors.NONE.code, 0)
  }


  def kafka2SpinocoData(in:collection.mutable.Map[TopicAndPartition, ByteBufferMessageSet])
  :Vector[(String @@ TopicName,Vector[(Int @@ PartitionId,Vector[spinoco.protocol.kafka.Message])])] = {

    in.toVector
    .map ({ case (tap,messages) =>
      val msgs =
      messages.iterator.toVector.map { case MessageAndOffset(msg, offset) =>
        val ts =
          if (msg.timestamp == Message.NoTimestamp) None
          else {
            msg.timestampType match {
              case TimestampType.NO_TIMESTAMP_TYPE => None
              case TimestampType.CREATE_TIME => Some(TimeData.CreateTime(new Date(msg.timestamp)))
              case TimestampType.LOG_APPEND_TIME => Some(TimeData.LogAppendTime(new Date(msg.timestamp)))
            }
          }
        val comp = msg.compressionCodec match {
          case GZIPCompressionCodec =>  Some(Compression.GZIP)
          case SnappyCompressionCodec => Some(Compression.Snappy)
          case LZ4CompressionCodec => Some(Compression.LZ4)
          case _ => None
        }

        comp match {
          case None =>
            spinoco.protocol.kafka.Message.SingleMessage(offset,MessageVersion(msg.magic),ts,if (msg.key == null) ByteVector.empty else ByteVector(msg.key), ByteVector(msg.payload))
          case Some(compression) => throw new Exception("Compressed messages to yet supported")
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

