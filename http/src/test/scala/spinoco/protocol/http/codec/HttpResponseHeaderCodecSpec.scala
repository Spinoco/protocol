package spinoco.protocol.http.codec

import java.time.ZonedDateTime

import org.scalacheck.Prop._
import org.scalacheck.Properties
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import spinoco.protocol.http.{HttpResponseHeader, HttpStatusCode, HttpVersion}
import spinoco.protocol.http.header._
import spinoco.protocol.http.header.value.{ProductDescription, ServerProduct}
import spinoco.protocol.mime.{ContentType, MediaType}


object HttpResponseHeaderCodecSpec extends Properties("HttpResponseHeaderCodec") {


  property("Response.decode") = secure {
    HttpResponseHeaderCodec.defaultCodec.decode(BitVector(
      Seq(
      "HTTP/1.1 200 OK"
      , "Date: Mon, 27 Jul 2009 12:28:53 GMT"
      , "Server: Apache/2.2.14"
      , "Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT"
      , "Content-Length: 88"
      , "Content-Type: text/html"
      , "Connection: Closed"
      ).mkString("\r\n").getBytes
    )) ?= Attempt.successful(DecodeResult(
      HttpResponseHeader(
        status =  HttpStatusCode.Ok
        , reason = "OK"
        , headers = List(
          Date(ZonedDateTime.parse("2009-07-27T12:28:53+00:00").toLocalDateTime)
          , Server(ServerProduct(List(ProductDescription("Apache",Some("2.2.14")))))
          , `Last-Modified`(ZonedDateTime.parse("2009-07-22T19:15:56+00:00").toLocalDateTime)
          , `Content-Length`(88)
          , `Content-Type`(ContentType.TextContent(MediaType.`text/html`, None))
          , Connection(List("Closed"))
        )
        , version = HttpVersion.V1_1
      )
    , BitVector.empty))
  }

  property("Response.encode") = secure {

    HttpResponseHeaderCodec.defaultCodec.encode(
      HttpResponseHeader(
        status =  HttpStatusCode.Ok
        , reason = "OK"
        , headers = List(
          Date(ZonedDateTime.parse("2009-07-27T12:28:53+00:00").toLocalDateTime)
          , Server(ServerProduct(List(ProductDescription("Apache",Some("2.2.14")))))
          , `Last-Modified`(ZonedDateTime.parse("2009-07-22T19:15:56+00:00").toLocalDateTime)
          , `Content-Length`(88)
          , `Content-Type`(ContentType.TextContent(MediaType.`text/html`, None))
          , Connection(List("Closed"))
        )
        , version = HttpVersion.V1_1
      )
    ).map(_.decodeAscii) ?= Attempt.successful(Right(
      Seq(
        "HTTP/1.1 200 OK"
        , "Date: Mon, 27 Jul 2009 12:28:53 GMT"
        , "Server: Apache/2.2.14"
        , "Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT"
        , "Content-Length: 88"
        , "Content-Type: text/html"
        , "Connection: Closed"
      ).mkString("\r\n")
    ))

  }

}
