package spinoco.protocol.http.codec

import scodec.Codec
import scodec.codecs._
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
      choice(
        (bytesUntil(_ != ' ').codedAs(HttpStatusCode.codec) <~ whitespace()) :: utf8String
        , HttpStatusCode.codec.xmap[HttpStatusCode :: String :: HNil](
          code => code :: code.description :: HNil
          , { case code :: _ :: HNil => code }
        )
      )
    }

    parametrizedN(crlf, crlf, "Response" | headerLineCodec, "Headers" | headerCodec).xmap[HttpResponseHeader] (
      { case (version :: status :: phrase :: HNil, headers) => HttpResponseHeader(status, phrase, headers, version) }
      , h => {
        val description =
          if (h.reason.isEmpty) h.status.description
          else h.reason

        ( h.version :: h.status :: description :: HNil, h.headers)
      }
    )
  }

}
