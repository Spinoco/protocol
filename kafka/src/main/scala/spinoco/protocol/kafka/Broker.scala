package spinoco.protocol.kafka

import shapeless.tag.@@

/**
  * Broker data
 *
  * @param nodeId     Id of the broker
  * @param host       Host (ip address) of the broker as defined in broker configuration
  * @param port       Port the broker accepts new connections at
  */
case class Broker(
  nodeId: Int @@ Broker
  , host: String
  , port: Int
)



