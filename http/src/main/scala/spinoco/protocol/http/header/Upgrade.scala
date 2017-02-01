package spinoco.protocol.http.header

import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.{HeaderCodecDefinition, ProductDescription}

/**
  *
  * RFC 2616 section 14.42
  *
  * @see https://tools.ietf.org/html/rfc2616#section-14.42
  */
case class Upgrade(products:List[ProductDescription]) extends DefaultHeader


object Upgrade { val codec =
  HeaderCodecDefinition[Upgrade]( commaDelimitedMin(ProductDescription.codec, 1).xmap (Upgrade.apply,_.products) )
}
