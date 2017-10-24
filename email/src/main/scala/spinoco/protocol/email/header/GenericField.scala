package spinoco.protocol.email.header

/**
  * Any field that is not parsed to EmailHeaderFiled types
  */
case class GenericField(name: String, value: String) extends EmailHeaderField
