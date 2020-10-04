package spinoco.protocol.http.header

/**
  * Generic unrecognized header
  */
case class GenericHeader(name: String, value: String) extends HttpHeader with ContentHeaderField

