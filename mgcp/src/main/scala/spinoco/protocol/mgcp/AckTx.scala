package spinoco.protocol.mgcp

import shapeless.tag.@@

case class AckTx(start: Int @@ MGCPTxId, end: Option[Int @@ MGCPTxId])

