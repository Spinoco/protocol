package spinoco.protocol.http.codec

import scodec.Codec
import spinoco.protocol.http._
import spinoco.protocol.http.header.HttpHeader
import spinoco.protocol.common.codec._
import helper._
import shapeless.{ ::, HNil }


object HttpResponseHeaderCodec {

  lazy val defaultCodec: Codec[HttpResponseHeader] =
    codec(HttpHeaderCodec.codec(maxHeaderLength = Int.MaxValue))

  def codec(headerCodec: Codec[HttpHeader]): Codec[HttpResponseHeader] = {
    val headerLineCodec: Codec[HttpVersion.Value :: HttpStatusCode :: String :: HNil] = {

      (bytesUntil(_ != ' ').codedAs(HttpVersion.codec) <~ whitespace()) ::
      (bytesUntil(_ != ' ').codedAs(HttpStatusCode.codec) <~ whitespace()) ::
      utf8String
    }

    parametrizedN(crlf, crlf, headerLineCodec, headerCodec).xmap[HttpResponseHeader] (
      { case (version :: status :: phrase :: HNil, headers) => HttpResponseHeader(status, phrase, headers, version) }
      , h => ( h.version :: h.status :: h.reason :: HNil, h.headers)
    )
  }

}
