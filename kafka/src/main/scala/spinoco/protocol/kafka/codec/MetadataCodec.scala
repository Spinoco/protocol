package spinoco.protocol.kafka.codec

import scodec.Codec
import shapeless.{::, HNil, tag}
import spinoco.protocol.kafka.Response.MetadataResponse
import spinoco.protocol.kafka._
import scodec.codecs._
import spinoco.protocol.common.util._
import shapeless.tag.@@
import spinoco.protocol.kafka.Request.MetadataRequest


object MetadataCodec {

  val requestCodec:Codec[MetadataRequest] = {
    val tagger = tagF[Vector,String]
    kafkaArray(kafkaRequiredString)
    .xmap(topics => MetadataRequest(tagger(topics)),rq => tagger.unwrap(rq.topics))
  }

  val metadataResponseCodec:Codec[MetadataResponse] = {

    (kafkaArray(impl.brokerCodec) ~ kafkaArray(impl.topicMetadataCodec))
    .xmap(MetadataResponse.apply _ tupled, mr => (mr.brokers, mr.topics))
  }

  object impl {
    val brokerCodec:Codec[Broker] = {
      "Broker" | (
        ("Node Id"      | int32) ::
        ("Host"         | kafkaRequiredString) ::
        ("Port"         | int32 )
      ).xmap(
        { case nodeId :: host :: port :: HNil => Broker(tag[Broker](nodeId),host,port)}
        , b => (b.nodeId:Int) :: b.host :: b.port :: HNil
      )
    }

    val partitionMetadataCodec:Codec[PartitionMetadata] = {
      "Partition" | (
        ("Error Code"       | kafkaError ) ::
        ("Partition Id"     | int32) ::
        ("Leader"           | int32) ::
        ("Replicas"         | kafkaArray(int32)) ::
        ("Isr"              | kafkaArray(int32))
      ).xmap(
        { case error :: pid :: leader :: replicas :: isrs :: HNil =>
          val leaderOption = if (leader == -1) None else Some(tag[Broker](leader))
          PartitionMetadata(
            error = error
            , id = tag[PartitionId](pid)
            , leader = leaderOption
            , replicas = replicas.asInstanceOf[Vector[Int @@ Broker]] // unsafe but saves vector traversal
            , isr = isrs.asInstanceOf[Vector[Int @@ Broker]] // unsafe but saves vector traversal
          )
        }
        , pm => pm.error :: (pm.id:Int) :: pm.leader.getOrElse(-1) :: (pm.replicas:Vector[Int]) :: (pm.isr:Vector[Int]) :: HNil
      )
    }


    val topicMetadataCodec:Codec[TopicMetadata] = {
      "Topic" | (
        ("Error Code"     | kafkaError) ::
        ("TopicName"      | kafkaRequiredString ) ::
        ("Partitions"     | kafkaArray(partitionMetadataCodec))
      ).xmap(
        { case error :: name :: partitions :: HNil =>
          TopicMetadata(error,tag[TopicName](name),partitions)
        }
        , tm => tm.error :: (tm.name:String) :: tm.partitions :: HNil
      )
    }
  }

}
