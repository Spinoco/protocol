package spinoco.protocol.mgcp


object BearerAttribute {

  case class BearerEncoding(tpe: BearerEncodingType.Value) extends BearerAttribute
  case class BearerExtensionName(nameAndValue: String) extends BearerAttribute

}


sealed trait BearerAttribute


object BearerEncodingType extends Enumeration {
  val ALaw = Value("A")
  val uLaw = Value("mu")
}