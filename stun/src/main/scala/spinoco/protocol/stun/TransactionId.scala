package spinoco.protocol.stun

import scodec.bits.ByteVector


sealed trait TransactionId {
  def toBytes:ByteVector
}



object TransactionId {

  private[stun] case class BigIntTransaction(txId:BigInt) extends TransactionId {
    def toBytes: ByteVector = ByteVector.view(txId.toByteArray)
  }

}