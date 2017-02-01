package spinoco.protocol.http.codec


import java.nio.charset.Charset

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import spinoco.protocol.http.header._
import spinoco.protocol.common.codec._
import spinoco.protocol.http.header.value.HeaderCodecDefinition


object HttpHeaderCodec {

  /**
    * Encodes// decodes arbitrary header line.
    * @param otherHeaders If you need to supply own headers to be encoded/decoded,
    *                     Supply them here. First is header name (may be upper/lowercase) and second
    *                     is decoder of the header.
    *                     This may also override default supplied codecs.
    * @return
    */
  def codec(otherHeaders: (String, Codec[HttpHeader]) *):Codec[HttpHeader] = {
    val allCodecs = allHeaderCodecs ++ otherHeaders.map { case (hdr,codec) => hdr.toLowerCase -> codec }.toMap
    implicit val ascii = Charset.forName("ASCII") // only ascii allowed in http header

    takeWhile(ByteVector(':'), ByteVector(':',' '), string, 1024).flatZip[HttpHeader] { name =>
      val trimmed = name.trim
      allCodecs.get(trimmed.toLowerCase) match {
        case Some(codec) => codec
        case None => utf8.xmap[GenericHeader](s => GenericHeader(trimmed, s), _.value).upcast[HttpHeader]
      }
    }.xmap (
      { case (name, header) => header }
      , (header:HttpHeader) => header.name -> header
    )
  }


  val allHeaderCodecs : Map[String, Codec[HttpHeader]] = Seq[HeaderCodecDefinition[HttpHeader]](
   Accept.codec
    , `Accept-Charset`.codec
    , `Accept-Encoding`.codec
    , `Accept-Language`.codec
    , `Accept-Ranges`.codec
    , `Access-Control-Allow-Credentials`.codec
    , `Access-Control-Allow-Headers`.codec
    , `Access-Control-Allow-Methods`.codec
    , `Access-Control-Allow-Origin`.codec
    , `Access-Control-Expose-Headers`.codec
    , `Access-Control-Max-Age`.codec
    , `Access-Control-Request-Headers`.codec
    , `Access-Control-Request-Method`.codec
    , Age.codec
    , `Cache-Control`.codec
    , Connection.codec
    , `Content-Disposition`.codec
    , `Content-Encoding`.codec
    , `Content-Length`.codec
    , `Content-Location`.codec
    , `Content-Type`.codec
    , Cookie.codec
    , Date.codec
    , ETag.codec
    , Expires.codec
    , Host.codec
    , `If-Match`.codec
    , `If-Modified-Since`.codec
    , `If-None-Match`.codec
    , `Keep-Alive`.codec
    , `Last-Modified`.codec
    , Location.codec
    , Origin.codec
    , Pragma.codec
    , Range.codec
    , Referer.codec
    , Server.codec
    , `Set-Cookie`.codec
    , `Sec-WebSocket-Accept`.codec
    , `Sec-WebSocket-Extensions`.codec
    , `Sec-WebSocket-Key`.codec
    , `Sec-WebSocket-Protocol`.codec
    , `Sec-WebSocket-Version`.codec
    , `Transfer-Encoding`.codec
    , Upgrade.codec
    , `Upgrade-Insecure-Requests`.codec
    , `User-Agent`.codec
    , `WWW-Authenticate`.codec
    , `X-Powered-By`.codec
  ).map { codec => codec.headerName.toLowerCase -> codec.headerCodec }.toMap


}
