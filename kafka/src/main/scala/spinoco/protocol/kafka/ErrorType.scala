package spinoco.protocol.kafka

/**
  * Created by pach on 17/07/16.
  */
object ErrorType extends Enumeration {

  val UNKNOWN = Value (-1)                          // The server experienced an unexpected error when processing the request 
  val OFFSET_OUT_OF_RANGE= Value (1)                // The requested offset is not within the range of offsets maintained by the server
  val CORRUPT_MESSAGE= Value (2)                    // This message has failed its CRC checksum, exceeds the valid size, or is otherwise corrupt.
  val UNKNOWN_TOPIC_OR_PARTITION= Value (3)         // This server does not host this topic-partition.
  val INVALID_FETCH_SIZE= Value (4)                 // The requested fetch size is invalid.
  val LEADER_NOT_AVAILABLE= Value (5)               // There is no leader for this topic-partition as we are in the middle of a leadership election.
  val NOT_LEADER_FOR_PARTITION= Value (6)           // This server is not the leader for that topic-partition.
  val REQUEST_TIMED_OUT= Value (7)                  // The request timed out.
  val BROKER_NOT_AVAILABLE= Value (8)               // The broker is not available.
  val REPLICA_NOT_AVAILABLE= Value (9)              // The replica is not available for the requested topic-partition
  val MESSAGE_TOO_LARGE= Value (10)                 // The request included a message larger than the max message size the server will accept.
  val STALE_CONTROLLER_EPOCH= Value (11)            // The controller moved to another broker.
  val OFFSET_METADATA_TOO_LARGE= Value (12)         // The metadata field of the offset request was too large.
  val NETWORK_EXCEPTION= Value (13)                 // The server disconnected before a response was received.
  val GROUP_LOAD_IN_PROGRESS= Value (14)            // The coordinator is loading and hence can't process requests for this group.
  val GROUP_COORDINATOR_NOT_AVAILABLE= Value (15)   // The group coordinator is not available.
  val NOT_COORDINATOR_FOR_GROUP= Value (16)         // This is not the correct coordinator for this group.
  val INVALID_TOPIC_EXCEPTION= Value (17)           // The request attempted to perform an operation on an invalid topic.
  val RECORD_LIST_TOO_LARGE= Value (18)             // The request included message batch larger than the configured segment size on the server.
  val NOT_ENOUGH_REPLICAS= Value (19)               // Messages are rejected since there are fewer in-sync replicas than required.
  val NOT_ENOUGH_REPLICAS_AFTER_APPEND= Value (20)  // Messages are written to the log, but to fewer in-sync replicas than required.
  val INVALID_REQUIRED_ACKS= Value (21)             // Produce request specified an invalid value for required acks.
  val ILLEGAL_GENERATION= Value (22)                // Specified group generation id is not valid.
  val INCONSISTENT_GROUP_PROTOCOL= Value (23)       // The group member's supported protocols are incompatible with those of existing members.
  val INVALID_GROUP_ID= Value (24)                  // The configured groupId is invalid
  val UNKNOWN_MEMBER_ID= Value (25)                 // The coordinator is not aware of this member.
  val INVALID_SESSION_TIMEOUT= Value (26)           // The session timeout is not within the range allowed by the broker (as configured by group.min.session.timeout.ms and group.max.session.timeout.ms
  val REBALANCE_IN_PROGRESS= Value (27)             // The group is rebalancing, so a rejoin is needed.
  val INVALID_COMMIT_OFFSET_SIZE= Value (28)        // The committing offset data size is not valid
  val TOPIC_AUTHORIZATION_FAILED= Value (29)        // Topic authorization failed.
  val GROUP_AUTHORIZATION_FAILED= Value (30)        // Group authorization failed.
  val CLUSTER_AUTHORIZATION_FAILED= Value (31)      // Cluster authorization failed.
  val INVALID_TIMESTAMP= Value (32)                 // The timestamp of the message is out of acceptable range.
  val UNSUPPORTED_SASL_MECHANISM= Value (33)        // The broker does not support the requested SASL mechanism.
  val ILLEGAL_SASL_STATE= Value (34)                // Request is not valid given the current SASL state.
  val UNSUPPORTED_VERSION= Value (35)               // The version of API is not supported.
  val TOPIC_ALREADY_EXISTS= Value (36)              // Topic with this name already exists.
  val INVALID_PARTITIONS= Value (37)                // Number of partitions is invalid.
  val INVALID_REPLICATION_FACTOR= Value (38)        // Replication-factor is invalid.
  val INVALID_REPLICA_ASSIGNMENT= Value (39)        // Replica assignment is invalid.
  val INVALID_CONFIG= Value (40)                    // Configuration is invalid.
  val NOT_CONTROLLER= Value (41)                    // This is not the correct controller for this cluster.
  val INVALID_REQUEST= Value (42)                   // This most likely occurs because of a request being malformed by the client library or the message was sent to an incompatible broker. See the broker logs for more details


}



 