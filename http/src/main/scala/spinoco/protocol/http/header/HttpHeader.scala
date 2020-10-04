package spinoco.protocol.http.header


/**
  *  Http Header.
  *
  *  Allows to specify Http header that has a header of `name` and value of `A`
  *
  *
  */

trait HttpHeader {

  def name: String

}

/** denotes header fields that may be used with MIME parts **/
trait ContentHeaderField extends HttpHeader