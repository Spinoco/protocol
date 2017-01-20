package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.codec.helper._


sealed case class ServerProduct(products: List[ProductDescription])

object ServerProduct {

  val codec : Codec[ServerProduct] = {
    listMultiplexed(
      _ ++ SP.bits ++ _
      , splitByWS
      , ProductDescription.codec
    ).xmap(ServerProduct.apply, _.products)
  }

}


sealed case class ProductDescription(name: String, comment: Option[String])


object ProductDescription {


  val codec: Codec[ProductDescription] = {
    parametrized[String, String](slash, utf8String, utf8String)
    .xmap(ProductDescription.apply _ tupled, pd => pd.name -> pd.comment)
  }

}