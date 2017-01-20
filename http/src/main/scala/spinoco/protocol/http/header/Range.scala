package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{ByteRange, HeaderCodecDefinition}



sealed case class Range(value:ByteRange) extends DefaultHeader

object Range { val codec =
  HeaderCodecDefinition[Range](ByteRange.codec.xmap (Range.apply, _.value))
}
