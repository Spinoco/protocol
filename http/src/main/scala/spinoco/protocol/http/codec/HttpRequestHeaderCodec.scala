package spinoco.protocol.http.codec

import scodec.{Attempt, Codec, Err}
import spinoco.protocol.http.{HttpMethod, HttpRequestHeader, HttpVersion, Uri}
import spinoco.protocol.http.header.HttpHeader
import spinoco.protocol.common.util._
import helper._
import scodec.bits.BitVector
import scodec.codecs._


object HttpRequestHeaderCodec {

  lazy val defaultCodec: Codec[HttpRequestHeader] =
    codec(HttpHeaderCodec.codec(maxHeaderLength = Int.MaxValue))

  def codec(headerCodec: Codec[HttpHeader]): Codec[HttpRequestHeader] = {
    // split by crlf, then process first line and headers
    val headerLineCodec: Codec[(HttpMethod.Value, Uri.Path, Uri.Query, HttpVersion.Value)] = {
      def decode(content:List[BitVector]):Attempt[(HttpMethod.Value, Uri.Path, Uri.Query, HttpVersion.Value)] = {
        content match {
          case methodBits :: pathBits :: versionBits :: Nil =>
            HttpMethod.codec.decodeValue(methodBits).flatMap { method =>
            Uri.pathQueryCodec.decodeValue(pathBits).flatMap { case (path, query) =>
            asciiString.decodeValue(versionBits).flatMap { versionString =>
            attempt(HttpVersion.withName(versionString.trim.drop(5))).map { version => // drop HTTP/
              (method, path, query, version)
            }}}}
          case _ =>
            Attempt.failure(Err(s"Method Uri HTTP expected, got ${content.map(_.decodeAscii)}"))
        }


      }


      def encode(method: HttpMethod.Value, uri: Uri.Path, query: Uri.Query, version: HttpVersion.Value): Attempt[List[BitVector]] = {
        HttpMethod.codec.encode(method).flatMap { methodBits =>
        Uri.pathQueryCodec.encode((uri,query)).map { pathBits =>
          List(methodBits, pathBits , BitVector.view(s"HTTP/$version".getBytes))
        }}
      }

      listMultiplexed(
        _ ++ SP.bits ++ _
        , splitByWS
        , bits
      ).exmap(decode,encode _ tupled)
    }

    parametrizedN(crlf, crlf, headerLineCodec, headerCodec).xmap[HttpRequestHeader] (
      { case ((method, path, query, version), headers) => HttpRequestHeader(method, path, headers, query, version) }
      , h => ((h.method, h.path, h.query, h.version), h.headers)
    )
  }


}
