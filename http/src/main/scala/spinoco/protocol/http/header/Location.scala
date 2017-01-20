package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, LocationDefinition}


/**
  *   RFC 7231 7.1.2
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Location
  */
sealed case class Location(value: LocationDefinition) extends DefaultHeader

object Location { val codec =
  HeaderCodecDefinition[Location](LocationDefinition.codec.xmap (Location.apply, _.value))
}

