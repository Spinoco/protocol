package spinoco.protocol

package object kafka {

  sealed trait PartitionId
  sealed trait BrokerId
  sealed trait TopicName
  sealed trait Offset


}
