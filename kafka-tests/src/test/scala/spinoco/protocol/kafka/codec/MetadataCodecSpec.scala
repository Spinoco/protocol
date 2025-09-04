package spinoco.protocol.kafka.codec


import kafka.api.{TopicMetadataRequest, TopicMetadataResponse}
import kafka.cluster.BrokerEndPoint
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


class MetadataCodecSpec extends CodecSpec {


  "Metadata API" - {

    "De-Serializes request" in forAll {
      (pv: ProtocolVersion.Value, clientId: String, topics: Vector[String], correlation: Int) =>

      MessageCodec.requestCodec.decode(
        serializeRequest(TopicMetadataRequest(pv.id.toShort,correlation,clientId,topics))
      ) shouldBe Attempt.successful(DecodeResult(
        RequestMessage(
          version = ProtocolVersion.Kafka_0_8
          , correlationId = correlation
          , clientId = clientId
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


    implicit val arbitraryBroker:Arbitrary[BrokerEndPoint] = Arbitrary {
      for {
        brokerId <- Gen.choose(0,100)
        host <- generators.ipString
        port <- Gen.choose(1024,32000)
      } yield BrokerEndPoint(brokerId,host,port)

    }

    implicit val arbitraryPartitionMeta:Arbitrary[kafka.api.PartitionMetadata] = Arbitrary {
      for {
        partitionId <- Gen.choose(0,100)
        leader <- The[Arbitrary[Option[BrokerEndPoint]]].arbitrary
        replicas <- The[Arbitrary[Seq[BrokerEndPoint]]].arbitrary
        isrs <- The[Arbitrary[Seq[BrokerEndPoint]]].arbitrary
        err <- Gen.choose(-1,35)
      } yield kafka.api.PartitionMetadata(partitionId,leader,replicas,isrs,err.toShort)
    }

    implicit val arbitraryTopicMetadata:Arbitrary[kafka.api.TopicMetadata] = Arbitrary {
      for {
        topicName <- The[Arbitrary[String]].arbitrary
        partitions <- The[Arbitrary[Seq[kafka.api.PartitionMetadata]]].arbitrary
        err <- Gen.choose(-1,35)
      } yield kafka.api.TopicMetadata(topicName,partitions,err.toShort)

    }

    def kafka2spinoco(brokers:Seq[BrokerEndPoint], meta:Seq[kafka.api.TopicMetadata]):MetadataResponse = {
      MetadataResponse(
        brokers = brokers.toVector.map { b => Broker(tag[Broker](b.id), b.host, b.port)}
        , topics = meta.toVector.map { tm =>
          TopicMetadata(
            error = if (tm.errorCode == 0) None else Some(ErrorType(tm.errorCode))
            , name = tag[TopicName](tm.topic)
            , partitions = tm.partitionsMetadata.toVector.map { pm =>
              PartitionMetadata(
                error = if (pm.errorCode == 0) None else Some(ErrorType(pm.errorCode))
                , id = tag[PartitionId](pm.partitionId)
                , leader = tagF(pm.leader.map(_.id))
                , replicas = tagF(pm.replicas.toVector.map(_.id))
                , isr = tagF(pm.isr.toVector.map(_.id))
              )
            }
          )
        }
      )
    }


    "De-Serializes response" in forAll {
      (version: ProtocolVersion.Value, brokers:Seq[BrokerEndPoint], meta:Seq[kafka.api.TopicMetadata], correlationId: Int) =>

        val request = TopicMetadataResponse(
          brokers = brokers
          , topicsMetadata = meta
          , correlationId = 2
        )

        val serialized = serializeResponse(request)

        MessageCodec.responseCorrelationCodec.decode(serialized).flatMap { result =>
          MessageCodec.responseCodecFor(version,ApiKey.MetadataRequest).decode(result.value._2)
        } shouldBe Attempt.successful(DecodeResult(
          kafka2spinoco(brokers,meta),BitVector.empty
        ))

    }


    "Serializes response" in forAll {
      (version: ProtocolVersion.Value, brokers:Seq[BrokerEndPoint], meta:Seq[kafka.api.TopicMetadata], correlationId: Int) =>

      val resp = kafka2spinoco(brokers,meta)
      MessageCodec.responseCodecFor(version,ApiKey.MetadataRequest).encode(resp).flatMap {
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
