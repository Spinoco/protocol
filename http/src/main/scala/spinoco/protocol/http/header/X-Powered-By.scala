package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.{HeaderCodecDefinition, ProductDescription}


case class `X-Powered-By`(product: ProductDescription) extends DefaultHeader

object `X-Powered-By` { val codec =
  HeaderCodecDefinition[`X-Powered-By` ](ProductDescription.codec.xmap (`X-Powered-By` .apply,_.product))
}