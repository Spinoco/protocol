package spinoco.protocol.kafka.codec


import java.util.Optional

import org.apache.kafka.common.protocol.Errors
import org.scalacheck.{Arbitrary, Gen}
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import spinoco.protocol.kafka.Request.MetadataRequest
import spinoco.protocol.kafka._
import org.scalacheck.Arbitrary._
import shapeless.tag
import shapeless.{the => The}
import spinoco.protocol.common.generators
import spinoco.protocol.kafka.Response.MetadataResponse
import spinoco.protocol.common.util._

import collection.JavaConverters._


class MetadataCodecSpec extends CodecSpec {


  "Metadata API" - {

    "De-Serializes request" in forAll {
      (topics: Vector[String]) =>

      val metadata = {
        new org.apache.kafka.common.requests.MetadataRequest.Builder(topics.toList.asJava, true, 1).build(1)
      }

      MessageCodec.requestCodec.decode(
        serializeRequest(metadata)
      ) shouldBe Attempt.successful(DecodeResult(
        RequestMessage(
          version = ProtocolVersion.Kafka_0_8
          , correlationId = 1
          , clientId = "client"
          , MetadataRequest(topics.map(tag[TopicName](_)))
        )
        , BitVector.empty
      ))

    }

    "Serializes request" in forAll {
      (pv: ProtocolVersion.Value, clientId:String, topics:Vector[String], correlation:Int) =>


      val msg = RequestMessage(
        version = pv
        , correlationId = correlation
        , clientId= clientId
        , MetadataRequest(topics.map(tag[TopicName](_)))
      )

      // hence there is no equivalent in kafka to read from this request,
      // we just verify that data are read/written correctly by our codec
      MessageCodec.requestCodec.encode(msg)
        .mapErr(err => fail(s"encoding failed $err"))
        .map { encoded =>
          MessageCodec.requestCodec.decode(encoded) shouldBe Attempt.successful(
            DecodeResult(msg.copy(version = ProtocolVersion.Kafka_0_8), BitVector.empty)
          )
        }
    }


    implicit val arbitraryBroker:Arbitrary[org.apache.kafka.common.Node] = Arbitrary {
      for {
        brokerId <- Gen.choose(0,100)
        host <- generators.ipString
        port <- Gen.choose(1024,32000)
      } yield new org.apache.kafka.common.Node(brokerId, host, port, "rack")

    }

    implicit val arbitraryPartitionMeta:Arbitrary[ org.apache.kafka.common.requests.MetadataResponse.PartitionMetadata] = Arbitrary {
      for {
        partitionId <- Gen.choose(0,100)
        leader <- The[Arbitrary[org.apache.kafka.common.Node]].arbitrary
        replicas <- The[Arbitrary[Seq[org.apache.kafka.common.Node]]].arbitrary
        isrs <- The[Arbitrary[Seq[org.apache.kafka.common.Node]]].arbitrary
      } yield new org.apache.kafka.common.requests.MetadataResponse.PartitionMetadata(Errors.NONE, partitionId, leader, Optional.empty[Integer](), replicas.toList.asJava, isrs.toList.asJava, List().asJava)
    }

    implicit val arbitraryTopicMetadata:Arbitrary[org.apache.kafka.common.requests.MetadataResponse.TopicMetadata] = Arbitrary {
      for {
        topicName <- The[Arbitrary[String]].arbitrary
        partitions <- The[Arbitrary[Seq[org.apache.kafka.common.requests.MetadataResponse.PartitionMetadata]]].arbitrary
      } yield new org.apache.kafka.common.requests.MetadataResponse.TopicMetadata(Errors.NONE, topicName, false, partitions.toList.asJava, 0)

    }

    def kafka2spinoco(brokers: Seq[org.apache.kafka.common.Node], meta: Seq[org.apache.kafka.common.requests.MetadataResponse.TopicMetadata]): MetadataResponse = {
      MetadataResponse(
        brokers = brokers.toVector.map { b => Broker(tag[Broker](b.id), b.host, b.port)}
        , topics = meta.toVector.map { tm =>
          TopicMetadata(
            error = if (tm.error() == Errors.NONE) None else Some(ErrorType(tm.error().code()))
            , name = tag[TopicName](tm.topic)
            , partitions = tm.partitionMetadata().asScala.toVector.map { pm =>
              PartitionMetadata(
                error = if (pm.error() == Errors.NONE) None else Some(ErrorType(pm.error().code()))
                , id = tag[PartitionId](pm.partition())
                , leader = tagF(Some(pm.leader.id()))
                , replicas = tagF(pm.replicas.asScala.toVector.map(_.id))
                , isr = tagF(pm.isr.asScala.toVector.map(_.id))
              )
            }
          )
        }
      )
    }


    "De-Serializes response" in forAll {
      (version: ProtocolVersion.Value, brokers: Seq[org.apache.kafka.common.Node], meta: Seq[org.apache.kafka.common.requests.MetadataResponse.TopicMetadata]) =>

        val request = org.apache.kafka.common.requests.MetadataResponse.prepareResponse(brokers.asJavaCollection, "clister1", 0, meta.toList.asJava)

        val serialized = serializeResponse(request, 0)

        MessageCodec.responseCorrelationCodec.decode(serialized).flatMap { result =>
          MessageCodec.responseCodecFor(version,ApiKey.MetadataRequest).decode(result.value._2)
        } shouldBe Attempt.successful(DecodeResult(
          kafka2spinoco(brokers,meta),BitVector.empty
        ))

    }


    "Serializes response" in forAll {
      (version: ProtocolVersion.Value, brokers:Seq[org.apache.kafka.common.Node], meta:Seq[org.apache.kafka.common.requests.MetadataResponse.TopicMetadata]) =>

      val resp = kafka2spinoco(brokers, meta)
      MessageCodec.responseCodecFor(version, ApiKey.MetadataRequest).encode(resp).flatMap {
        encoded => MessageCodec.responseCorrelationCodec.encode((ApiKey.MetadataRequest.id,encoded))
      }
      .mapErr(err => fail(s"Failed to encode MetdataResponse: $err"))
      .flatMap { encoded =>
        MessageCodec.responseCorrelationCodec.decode(encoded).flatMap { result =>
          MessageCodec.responseCodecFor(version,ApiKey.MetadataRequest).decode(result.value._2)
        }
      } shouldBe Attempt.successful(DecodeResult(
        resp,BitVector.empty
      ))


    }

  }

}
