package spinoco.protocol.asn.ber

/** The scope of the BER encoded tag. */
object BerClass extends Enumeration {

  val Universal = Value(0)
  val Application = Value(1)
  val Context = Value(2)
  val Private = Value(3)

}
