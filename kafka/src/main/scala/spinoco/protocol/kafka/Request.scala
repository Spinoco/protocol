package spinoco.protocol.kafka

import java.util.Date

import shapeless.tag.@@

import scala.concurrent.duration.FiniteDuration

/**
  * Request for Kafka Broker
  */
sealed trait Request


object Request {

  /**
    * Request to get information about given topic.
    * Note: If "auto.create.topics.enable" is set in the broker configuration,
    * a topic metadata request will create the topic with the default replication factor and number of partitions.
    * @param topics    Name of topic(s) to query metadata from
    */
  case class MetadataRequest(
     topics:Vector[String @@ TopicName]
  ) extends Request

  /**
    * The produce API is used to send message sets to the server.
    * For efficiency it allows sending message sets intended for many topic partitions in a single request.
    *
    * The produce API uses the generic message set format, but since no offset has been assigned to the messages
    * at the time of the send the producer is free to fill in that field in any way it likes.
    *
    * @param requiredAcks  This field indicates how many acknowledgements the servers should receive
    *                      before responding to the request.
    *                      If it is 0 the server will not send any response
    *                      (this is the only case where the server will not reply to a request).
    *                      If it is 1, the server will wait the data is written to the local log before sending a response.
    *                      If it is -1 the server will block until the message is committed by all in sync replicas before sending a response.
    *
    * @param timeout      This provides a maximum time  the server can await the receipt of the number
    *                     of acknowledgements in RequiredAcks. The timeout is not an exact limit
    *                     on the request time for a few reasons:
    *                     (1) it does not include network latency,
    *                     (2) the timer begins at the beginning of the processing of this request so if many requests
    *                     are queued due to server overload that wait time will not be included,
    *                     (3) we will not terminate a local write so if the local write time exceeds
    *                     this timeout it will not be respected.
    *                     To get a hard timeout of this type the client should use the socket timeout.
    *
    * @param messages     Messages, organized by topic and partition
    *
    */
  case class ProduceRequest(
    requiredAcks: RequiredAcks.Value
    , timeout: FiniteDuration
    , messages: Vector[(String @@ TopicName,Vector[(Int @@ PartitionId,Vector[Message])])]
  ) extends Request

  object RequiredAcks extends Enumeration {
    val NoResponse = Value(0)   // do not send response for this message
    val LocalOnly = Value(1)    // only commit in local node
    val Quorum = Value(-1)      // await full quorum of in sync replicas to confirm the message
  }


  /**
    * The fetch API is used to fetch a chunk of one or more logs for some topic-partitions.
    * Logically one specifies the topics, partitions, and starting offset at which to begin the fetch and gets back a chunk of messages.
    *
    * In general, the return messages will have offsets larger than or equal to the starting offset.
    * However, with compressed messages, it's possible for the returned messages to have offsets smaller than the starting offset.
    * The number of such messages is typically small and the caller is responsible for filtering out those messages.
    *
    * Fetch requests follow a long poll model so they can be made to block for a period of time if sufficient data is not immediately available.
    * As an optimization the server is allowed to return a partial message at the end of the message set. Clients should handle this case.
    *
    * @param replica          The replica id indicates the node id of the replica initiating this request.
    *                         Normal client consumers should always specify this as -1 as they have no node id.
    *                         Other brokers set this to be their own node id. The value -2 is accepted to allow
    *                         a non-broker to issue fetch requests as if it were a replica broker for debugging purposes.
    *
    * @param maxWaitTime      The max wait time is the maximum amount of time in milliseconds to block waiting
    *                         if insufficient data is available at the time the request is issued.
    *
    * @param minBytes         This is the minimum number of bytes of messages that must be available to give a response.
    *                         If the client sets this to 0 the server will always respond immediately,
    *                         however if there is no new data since their last request they will just get back empty message sets.
    *                         If this is set to 1, the server will respond as soon as at least one partition
    *                         has at least 1 byte of data or the specified timeout occurs.
    *
    *                         By setting higher values in combination with the timeout the consumer can tune for throughput
    *                         and trade a little additional latency for reading only large chunks of data
    *                         (e.g. setting MaxWaitTime to 100 ms and setting MinBytes to 64k would allow the server
    *                         to wait up to 100ms to try to accumulate 64k of data before responding).
    *
    * @param topics          Specifies name of topic, its partition and offset to read from. The last Int specifies
    *                         max bytes to read, that bounds message size received.
    */
  case class FetchRequest(
   replica: Int @@ Broker
   , maxWaitTime: FiniteDuration
   , minBytes: Int
   , topics:Vector[(String @@ TopicName,Vector[(Int @@ PartitionId, Long @@ Offset, Int)])]
   ) extends Request


  /**
    * Requests actual last known offsets for topic and partition. This allows to query last offset that has been
    * committed to the broker.
    * @param topics           Query for given topics and partitions. Supplied time (kafka 0.10.1+) is used to ask for all messages before a certain time (ms). FOr earlier versions this is ignored.
    *                         There are two special values.
    *                         Specify -1 to receive the latest offset (i.e. the offset of the next coming message) and -2 to receive the earliest available offset.
    *                         The last Int indicates maximum number of offset chunks to return. Only availabel for kafka protocol 0.8 and 0.9.
    *                         This applies to all versions of the API. Note that because offsets are pulled in descending order, asking for the earliest offset will always return you a single element.
    * @param replicaId        Supply -1 for client requests
    */
  case class OffsetsRequest(
    replicaId: Int @@ Broker
    , topics: Vector[(String @@ TopicName, Vector[(Int @@ PartitionId, Date, Option[Int])])]
  ) extends Request



}


