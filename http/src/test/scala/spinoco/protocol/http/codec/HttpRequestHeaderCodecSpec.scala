package spinoco.protocol.http.codec

import org.scalacheck.Prop._
import org.scalacheck.Properties
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import spinoco.protocol.http.header._
import spinoco.protocol.http._
import spinoco.protocol.http.header.value._



object HttpRequestHeaderCodecSpec extends Properties("HttpRequestHeaderCodec")  {

  property("Request.decode") = secure {
    HttpRequestHeaderCodec.defaultCodec.decode(BitVector(
      Seq(
        "GET /hello.htm?a=b&c=d HTTP/1.1"
        , "User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)"
        , "Host: www.tutorialspoint.com"
        , "Accept-Language: en-us"
        , "Accept-Encoding: gzip, deflate"
        , "Connection: Keep-Alive"
      ).mkString("\r\n").getBytes
    )) ?= Attempt.successful(DecodeResult(
      HttpRequestHeader(
        method = HttpMethod.GET
        , path = Uri.Path / "hello.htm"
        , query = Uri.Query(List("a" ->"b", "c" -> "d"))
        , headers = List(
          `User-Agent`(AgentVersion("Mozilla/4.0 (compatible; MSIE5.01; Windows NT)"))
          , Host(HostPort("www.tutorialspoint.com", None))
          , `Accept-Language`(List(LanguageRange.One(Language("en", Some("us")), None)))
          , `Accept-Encoding`(List(HttpEncodingRange.One(HttpEncoding("gzip"), None), HttpEncodingRange.One(HttpEncoding("deflate"), None)))
          , Connection(List("Keep-Alive"))
        )
        , version = HttpVersion.V1_1
      )
    , BitVector.empty))
  }

  property("Request.encode") = secure {
    HttpRequestHeaderCodec.defaultCodec.encode(
      HttpRequestHeader(
        method = HttpMethod.GET
        , path = Uri.Path / "hello.htm"
        , query = Uri.Query(List("a" ->"b", "c" -> "d"))
        , headers = List(
          `User-Agent`(AgentVersion("Mozilla/4.0 (compatible; MSIE5.01; Windows NT)"))
          , Host(HostPort("www.tutorialspoint.com", None))
          , `Accept-Language`(List(LanguageRange.One(Language("en", Some("us")), None)))
          , `Accept-Encoding`(List(HttpEncodingRange.One(HttpEncoding("gzip"), None), HttpEncodingRange.One(HttpEncoding("deflate"), None)))
          , Connection(List("Keep-Alive"))
        )
        , version = HttpVersion.V1_1
      )
    ).map(_.decodeAscii) ?= Attempt.successful(Right(
      Seq(
      "GET /hello.htm?a=b&c=d HTTP/1.1"
      , "User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)"
      , "Host: www.tutorialspoint.com"
      , "Accept-Language: en-us"
      , "Accept-Encoding: gzip, deflate"
      , "Connection: Keep-Alive"
      ).mkString("\r\n")
    ))

  }

}
