package spinoco.protocol.http.codec

import java.time.ZonedDateTime

import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}
import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, DecodeResult}
import spinoco.protocol.http.header.value.HttpMediaRange
import spinoco.protocol.http.Uri._
import spinoco.protocol.http.header._
import spinoco.protocol.http.header.value._
import spinoco.protocol.http._
import spinoco.protocol.mime.ContentType._
import spinoco.protocol.mime.MediaType.CustomMediaType
import spinoco.protocol.mime._

import scala.concurrent.duration._


object HeaderCodecSpec extends Properties("HeaderCodec") {


  val codec = HttpHeaderCodec.codec(maxHeaderLength = Int.MaxValue)

  def checkExamples(examples:Seq[(String, HttpHeader, String)]):Prop = {
    examples.foldLeft(true:Prop) {
      case (prop, (example, header, encoded)) =>
        prop && (("Decode: " + example) |: (
          codec.decode(BitVector.view(example.getBytes())) ?= Attempt.Successful(
            DecodeResult(header, BitVector.empty)
          )
        )) && (("Encode: " + encoded) |: (
          codec.encode(header).map(_.decodeAscii.fold(_.getMessage, identity)) ?= Attempt.Successful(encoded)
        ))
    }
  }

  property("Accept Header") = secure {
    checkExamples(Seq(
      ("Accept: text/html", Accept(List(HttpMediaRange.One(MediaType.`text/html`, None))), "Accept: text/html")
      , ("Accept: image/*", Accept(List(HttpMediaRange.Pattern("image", None))), "Accept: image/*")
      , ("Accept: */*", Accept(List(HttpMediaRange.Pattern("*", None))), "Accept: */*")
      , ("Accept: text/html, application/xhtml+xml, application/xml;q=0.9, */*;q=0.8"
         , Accept(List(
            HttpMediaRange.One(MediaType.`text/html`, None)
            , HttpMediaRange.One(MediaType.`application/xhtml+xml`, None)
            , HttpMediaRange.One(MediaType.`application/xml`, Some(0.9f))
            , HttpMediaRange.Pattern("*", Some(0.8f))
          ))
         ,"Accept: text/html, application/xhtml+xml, application/xml;q=0.9, */*;q=0.8"
        )
      , ("Accept: text/html,\n application/xhtml+xml,application/xml;q=0.9,\t\n\t */*;q=0.8"
        , Accept(List(
        HttpMediaRange.One(MediaType.`text/html`, None)
        , HttpMediaRange.One(MediaType.`application/xhtml+xml`, None)
        , HttpMediaRange.One(MediaType.`application/xml`, Some(0.9f))
        , HttpMediaRange.Pattern("*", Some(0.8f))
      ))
        ,"Accept: text/html, application/xhtml+xml, application/xml;q=0.9, */*;q=0.8"
        )
      , ("Accept: text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2"
        , Accept(List(
          HttpMediaRange.One(MediaType.`text/html`, None)
          , HttpMediaRange.One(MediaType.`image/gif`, None)
          , HttpMediaRange.One(MediaType.`image/jpeg`, None)
          , HttpMediaRange.Pattern("*", Some(0.2f))
          , HttpMediaRange.Pattern("*", Some(0.2f))
      ))
        , "Accept: text/html, image/gif, image/jpeg, */*;q=0.2, */*;q=0.2")
      , ("Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3"
        , Accept(List(
        HttpMediaRange.One(MediaType.`text/html`, None)
        , HttpMediaRange.One(MediaType.`application/xhtml+xml`, None)
        , HttpMediaRange.One(MediaType.`application/xml`, Some(0.9f))
        , HttpMediaRange.One(MediaType.`image/webp`, None)
        , HttpMediaRange.One(CustomMediaType("image", "apng"), None)
        , HttpMediaRange.Pattern("*", Some(0.8f))
        , HttpMediaRange.One(CustomMediaType("application", "signed-exchange"), None, params = Map("v" -> "b3"))
      ))
        ,"Accept: text/html, application/xhtml+xml, application/xml;q=0.9, image/webp, image/apng, */*;q=0.8, application/signed-exchange;v=b3"
      )
    ))
  }

  property("Accept-Charset Header") = secure {
    import HttpCharsetRange._

    checkExamples(Seq(
      ("Accept-Charset: iso-8859-1",`Accept-Charset`(List(One(MIMECharset.`ISO-8859-1`, None))),"Accept-Charset: iso-8859-1")
      , ("Accept-Charset: utf-8, iso-8859-1;q=0.5",`Accept-Charset`(List(One(MIMECharset.`UTF-8`, None), One(MIMECharset.`ISO-8859-1`, Some(0.5f)))),"Accept-Charset: utf-8, iso-8859-1;q=0.5")
      , ("Accept-Charset: utf-8, iso-8859-1;q=0.5",`Accept-Charset`(List(One(MIMECharset.`UTF-8`, None), One(MIMECharset.`ISO-8859-1`, Some(0.5f)))),"Accept-Charset: utf-8, iso-8859-1;q=0.5")
      , ("Accept-Charset: utf-8, iso-8859-1;q=0.5, *;q=0.1",`Accept-Charset`(List(One(MIMECharset.`UTF-8`, None), One(MIMECharset.`ISO-8859-1`, Some(0.5f)), Any(Some(0.1f)))),"Accept-Charset: utf-8, iso-8859-1;q=0.5, *;q=0.1")
    ))
  }


  property("Accept-Encoding Header") = secure {
    import HttpEncodingRange._

    checkExamples(Seq(
      ("Accept-Encoding: gzip",`Accept-Encoding`(List(One(HttpEncoding("gzip"), None))),"Accept-Encoding: gzip")
      , ("Accept-Encoding: deflate, gzip;q=1.0, *;q=0.5",`Accept-Encoding`(List(One(HttpEncoding("deflate"), None), One(HttpEncoding("gzip"), Some(1.0f)), Any(Some(0.5f)))),"Accept-Encoding: deflate, gzip;q=1.0, *;q=0.5")
    ))
  }

  property("Accept-Language Header") = secure {
    import LanguageRange._

    checkExamples(Seq(
      ("Accept-Language: de",`Accept-Language`(List(One(Language("de", None), None))), "Accept-Language: de")
      , ("Accept-Language: *",`Accept-Language`(List(Any(None))), "Accept-Language: *")
      , ("Accept-Language: en-US,en;q=0.5",`Accept-Language`(List(One(Language("en", Some("US")), None), One(Language("en", None), Some(0.5f)))),"Accept-Language: en-US, en;q=0.5")
    ))
  }


property("Accept-Ranges Header") = secure {
  checkExamples(Seq(
    ("Accept-Ranges: none",`Accept-Ranges`(None), "Accept-Ranges: none")
    , ("Accept-Ranges: bytes",`Accept-Ranges`(Some(RangeUnit.Bytes)), "Accept-Ranges: bytes")
  ))
}
  property("Access-Control-Allow-Credentials Header") = secure {
    checkExamples(Seq(
      ("Access-Control-Allow-Credentials: true",`Access-Control-Allow-Credentials`(true), "Access-Control-Allow-Credentials: true")
      , ("Access-Control-Allow-Credentials: false",`Access-Control-Allow-Credentials`(false), "Access-Control-Allow-Credentials: false")
    ))
  }

  property("Access-Control-Allow-Headers Header") = secure {
    checkExamples(Seq(
      ("Access-Control-Allow-Headers: X-Custom-Header",`Access-Control-Allow-Headers` (List("X-Custom-Header")), "Access-Control-Allow-Headers: X-Custom-Header")
      , ("Access-Control-Allow-Headers: X-Custom-Header, \nAccept",`Access-Control-Allow-Headers` (List("X-Custom-Header", "Accept")), "Access-Control-Allow-Headers: X-Custom-Header, Accept")
    ))
  }

  property("Access-Control-Allow-Methods Header") = secure {
    checkExamples(Seq(
      ("Access-Control-Allow-Methods: POST",`Access-Control-Allow-Methods` (List(HttpMethod.POST)), "Access-Control-Allow-Methods: POST")
      , ("Access-Control-Allow-Methods: POST, GET, OPTIONS",`Access-Control-Allow-Methods` (List(HttpMethod.POST, HttpMethod.GET, HttpMethod.OPTIONS)), "Access-Control-Allow-Methods: POST, GET, OPTIONS")
    ))
  }

  property("Access-Control-Allow-Origin Header") = secure {
    checkExamples(Seq(
      ("Access-Control-Allow-Origin: *",`Access-Control-Allow-Origin` (HttpOrigin.Any), "Access-Control-Allow-Origin: *")
      , ("Access-Control-Allow-Origin: https://developer.mozilla.org"
        ,`Access-Control-Allow-Origin` (HttpOrigin.One("https", Some(HostPort("developer.mozilla.org", None))))
        , "Access-Control-Allow-Origin: https://developer.mozilla.org")
      , ("Access-Control-Allow-Origin: http://developer.mozilla.org:8080"
        ,`Access-Control-Allow-Origin` (HttpOrigin.One("http", Some(HostPort("developer.mozilla.org", Some(8080)))))
        , "Access-Control-Allow-Origin: http://developer.mozilla.org:8080")
    ))
  }

  property("Access-Control-Expose-Headers Header") = secure {
    checkExamples(Seq(
      ("Access-Control-Expose-Headers: Content-Type",`Access-Control-Expose-Headers` (List("Content-Type")), "Access-Control-Expose-Headers: Content-Type")
      , ("Access-Control-Expose-Headers: X-PINGOTHER, Content-Type",`Access-Control-Expose-Headers` (List("X-PINGOTHER", "Content-Type")), "Access-Control-Expose-Headers: X-PINGOTHER, Content-Type")
    ))
  }

  property("Access-Control-Request-Headers Header") = secure {
    checkExamples(Seq(
      ("Access-Control-Request-Headers: Content-Type",`Access-Control-Request-Headers` (List("Content-Type")), "Access-Control-Request-Headers: Content-Type")
      , ("Access-Control-Request-Headers: X-PINGOTHER, Content-Type",`Access-Control-Request-Headers` (List("X-PINGOTHER", "Content-Type")), "Access-Control-Request-Headers: X-PINGOTHER, Content-Type")
    ))
  }

  property("Access-Control-Max-Age Header") = secure {
    checkExamples(Seq(
      ("Access-Control-Max-Age: 600",`Access-Control-Max-Age` (10.minutes), "Access-Control-Max-Age: 600")
    ))
  }

  property("Access-Control-Request-Method Header") = secure {
    checkExamples(Seq(
      ("Access-Control-Request-Method: POST",`Access-Control-Request-Method` (HttpMethod.POST), "Access-Control-Request-Method: POST")
    ))
  }

  property("Age Header") = secure {
    checkExamples(Seq(
      ("Age: 24", Age(24.seconds), "Age: 24")
    ))
  }

  property("Authorization Header") = secure {
    checkExamples(Seq(
      ("Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
        , Authorization(HttpCredentials.BasicHttpCredentials("Aladdin", "open sesame"))
        , "Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
      , ("Authorization: Bearer mF_9.B5f-4.1JqM"
        , Authorization(HttpCredentials.OAuthToken("Bearer", "mF_9.B5f-4.1JqM"))
        , "Authorization: Bearer mF_9.B5f-4.1JqM")
      , ("Authorization: Digest username=\"Mufasa\",\n                 realm=\"testrealm@host.com\",\n                 nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\",\n                 uri=\"/dir/index.html\",\n                 qop=auth,\n                 nc=00000001,\n         cnonce=\"0a4f113b\",\n                 response=\"6629fae49393a05397450978507c4ef1\",\n                 opaque=\"5ccc069c403ebaf9f0171e9517f40e41\""
        , Authorization(HttpCredentials.DigestHttpCredentials("Digest", Map(
        "username" -> "Mufasa"
        , "realm" -> "testrealm@host.com"
        , "nonce" -> "dcd98b7102dd2f0e8b11d0f600bfb0c093"
        , "uri" -> "/dir/index.html"
        , "qop" -> "auth"
        , "nc" -> "00000001"
        , "cnonce" -> "0a4f113b"
        , "response" -> "6629fae49393a05397450978507c4ef1"
        , "opaque" -> "5ccc069c403ebaf9f0171e9517f40e41"
      )))
        , "Authorization: Digest nc=00000001, nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093, username=Mufasa, uri=\"/dir/index.html\", cnonce=0a4f113b, qop=auth, response=6629fae49393a05397450978507c4ef1, opaque=5ccc069c403ebaf9f0171e9517f40e41, realm=\"testrealm@host.com\"")
    ))
  }

  property("Cache-Control Header") = secure {
    checkExamples(Seq(
      ("Cache-Control: no-cache, no-store, must-revalidate"
        , `Cache-Control`(List(CacheDirective.`no-cache`, CacheDirective.`no-store`, CacheDirective.`must-revalidate`))
        , "Cache-Control: no-cache, no-store, must-revalidate")
      , ("Cache-Control: public, max-age=31536000"
        , `Cache-Control`(List(CacheDirective.`public`, CacheDirective.`max-age`(31536000.seconds)))
        , "Cache-Control: public, max-age=31536000")
    ))
  }

  property("Connection Header") = secure {
    checkExamples(Seq(
      ("Connection: close", Connection(Nil), "Connection: close")
      , ("Connection: keep-alive", Connection(List("keep-alive")), "Connection: keep-alive")
      , ("Connection: keep-alive, X-Header", Connection(List("keep-alive", "X-Header")), "Connection: keep-alive, X-Header")
    ))
  }


  property("Content-Disposition Header") = secure {
    checkExamples(Seq(
      ("Content-Disposition: form-data", `Content-Disposition`(ContentDisposition(ContentDispositionType.IETFToken("form-data"), Map.empty)), "Content-Disposition: form-data")
      , ("Content-Disposition: attachment; filename=file%20name.jpg"
        , `Content-Disposition`(ContentDisposition(ContentDispositionType.Attachment, Map("filename" -> "file name.jpg")))
        , "Content-Disposition: attachment; filename=file%20name.jpg")
      , ("Content-Disposition: form-data; name=\"fieldName\"; filename=\"file%20name.jpg\""
        , `Content-Disposition`(ContentDisposition(ContentDispositionType.IETFToken("form-data"), Map("name" -> "fieldName", "filename" -> "file name.jpg")))
        , "Content-Disposition: form-data; name=fieldName; filename=file%20name.jpg")
      , ("Content-Disposition: attachment; filename=oneWord.jpg"
        , `Content-Disposition`(ContentDisposition(ContentDispositionType.Attachment, Map("filename" -> "oneWord.jpg")))
        , "Content-Disposition: attachment; filename=oneWord.jpg")
    ))
  }


  property("Content-Encoding Header") = secure {
    checkExamples(Seq(
      ("Content-Encoding: gzip",`Content-Encoding` (HttpEncoding("gzip")), "Content-Encoding: gzip")
    ))
  }

  property("Content-Length Header") = secure {
    checkExamples(Seq(
      ("Content-Length: 1024",`Content-Length` (1024), "Content-Length: 1024")
    ))
  }


  property("Content-Location Header") = secure {
    checkExamples(Seq(
      ("Content-Location: /that/location/ ",`Content-Location` (Uri.Path / "that" / "location" /), "Content-Location: /that/location/")
    ))
  }

  property("Content-Range Header") = secure {
    checkExamples(Seq(
      ("Content-Range: bytes 0-100/1000" , `Content-Range`(0,100,Some(1000)) , "Content-Range: bytes 0-100/1000")
      , ("Content-Range: bytes 100-200/1000" , `Content-Range`(100,200,Some(1000)) , "Content-Range: bytes 100-200/1000")
      , ("Content-Range: bytes 500-1000/1000" , `Content-Range`(500,1000,Some(1000)) , "Content-Range: bytes 500-1000/1000")
      , ("Content-Range: bytes 500-1000/*" , `Content-Range`(500,1000,None) , "Content-Range: bytes 500-1000/*")
    ))
  }


  property("Content-Type Header") = secure {
    checkExamples(Seq(
      ("Content-Type: audio/ogg",`Content-Type` (BinaryContent(MediaType.`audio/ogg`, None)), "Content-Type: audio/ogg")
      , ("Content-Type: text/html; charset=utf-8",`Content-Type` (TextContent(MediaType.`text/html`, Some(MIMECharset.`UTF-8`))), "Content-Type: text/html; charset=utf-8")
      , ("Content-Type: multipart/form-data; boundary=something"
        ,`Content-Type` (MultiPartContent(MediaType.`multipart/form-data`.copy(parameters = Map("boundary" -> "something")), None))
        , """Content-Type: multipart/form-data; boundary="something""""
        )
    ))
  }


  property("Cookie Header") = secure {
    checkExamples(Seq(
      ("Cookie: SESSID=298zf09hf012fh2; csrftoken=u32t4o3tb3gg43; _gat=1"
        , Cookie(HttpCookie("SESSID", "298zf09hf012fh2", None, None, None, None, false, false, Map("csrftoken" -> "u32t4o3tb3gg43", "_gat" -> "1")))
        , "Cookie: SESSID=298zf09hf012fh2; csrftoken=u32t4o3tb3gg43; _gat=1")
      , ("Cookie: SESSID=298zf09hf012fh2; Max-Age=300"
        , Cookie(HttpCookie("SESSID", "298zf09hf012fh2", None, Some(300.seconds), None, None, false, false, Map.empty))
        , "Cookie: SESSID=298zf09hf012fh2; Max-Age=300")
      , ("Cookie: SESSID=298zf09hf012fh2; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
        , Cookie(HttpCookie("SESSID", "298zf09hf012fh2", Some(ZonedDateTime.parse("1994-11-06T08:49:37+00:00").toLocalDateTime), None, None, None, false, false, Map.empty))
        , "Cookie: SESSID=298zf09hf012fh2; Expires=Sun, 6 Nov 1994 08:49:37 GMT")
      , ("Cookie: SESSID=298zf09hf012fh2; Domain=that.domain.com"
        , Cookie(HttpCookie("SESSID", "298zf09hf012fh2", None, None, Some("that.domain.com"), None, false, false, Map.empty))
        , "Cookie: SESSID=298zf09hf012fh2; Domain=that.domain.com")
      , ("Cookie: SESSID=298zf09hf012fh2; Path=/some/path"
        , Cookie(HttpCookie("SESSID", "298zf09hf012fh2", None, None, None, Some("/some/path"), false, false, Map.empty))
        , "Cookie: SESSID=298zf09hf012fh2; Path=/some/path")
      , ("Cookie: SESSID=298zf09hf012fh2; secure"
        , Cookie(HttpCookie("SESSID", "298zf09hf012fh2", None, None, None, None, secure = true, false, Map.empty))
        , "Cookie: SESSID=298zf09hf012fh2; secure")
      , ("Cookie: SESSID=298zf09hf012fh2; httponly"
        , Cookie(HttpCookie("SESSID", "298zf09hf012fh2", None, None, None, None, false, httpOnly = true, Map.empty))
        , "Cookie: SESSID=298zf09hf012fh2; httponly")
      , ("Cookie: __utmz=118721.140907.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); Currency=%22usd%22"
        , Cookie(HttpCookie("__utmz", "118721.140907.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)", None, None, None, None, false, false, Map("Currency" -> "%22usd%22")))
        , "Cookie: __utmz=118721.140907.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); Currency=%22usd%22")
    ))
  }

  property("Date Header") = secure {
    checkExamples(Seq(
      ("Date: Wed, 21 Oct 2015 07:28:00 GMT", Date(ZonedDateTime.parse("2015-10-21T07:28:00+00:00").toLocalDateTime), "Date: Wed, 21 Oct 2015 07:28:00 GMT")
    ))
  }

  property("ETag Header") = secure {
    checkExamples(Seq(
      ("ETag: \"33a64df551425fcc55e4d42a148795d9f25f89d4\""
        , ETag(EntityTag("33a64df551425fcc55e4d42a148795d9f25f89d4", weak = false))
        , "ETag: \"33a64df551425fcc55e4d42a148795d9f25f89d4\"")
      , ("ETag: W/\"0815\"", ETag(EntityTag("0815", weak = true)), "ETag: W/\"0815\"")
    ))
  }


  property("Expires Header") = secure {
    checkExamples(Seq(
      ("Expires: Wed, 21 Oct 2015 07:28:00 GMT", Expires(Right(ZonedDateTime.parse("2015-10-21T07:28:00+00:00").toLocalDateTime)), "Expires: Wed, 21 Oct 2015 07:28:00 GMT")
      , ("Expires: mon, 26 jul 1997 05:00:00 gmt", Expires(Left(0)), "Expires: 0")  // invalid format, respective date
      , ("Expires: -1", Expires(Left(-1)), "Expires: -1")
    ))
  }

  property("Host Header") = secure {
    checkExamples(Seq(
      ("Host: developer.cdn.mozilla.net", Host(HostPort("developer.cdn.mozilla.net", None)), "Host: developer.cdn.mozilla.net")
      , ("Host: developer.cdn.mozilla.net:8080", Host(HostPort("developer.cdn.mozilla.net", Some(8080))), "Host: developer.cdn.mozilla.net:8080")
    ))
  }

  property("If-Match Header") = secure {
    checkExamples(Seq(
      ("If-Match: *", `If-Match`(EntityTagRange.Any), "If-Match: *")
      , ("If-Match: \"bfc13a64729c4290ef5b2c2730249c88ca92d82d\""
        , `If-Match`(EntityTagRange.Range(List(EntityTag("bfc13a64729c4290ef5b2c2730249c88ca92d82d", weak = false))))
        , "If-Match: \"bfc13a64729c4290ef5b2c2730249c88ca92d82d\"")
      , ("If-Match: W/\"67ab43\", \"54ed21\", \"7892dd\""
        , `If-Match`(EntityTagRange.Range(List(EntityTag("67ab43", weak = true), EntityTag("54ed21", weak = false), EntityTag("7892dd", weak = false))))
        , "If-Match: W/\"67ab43\", \"54ed21\", \"7892dd\"")
    ))
  }

  property("If-Modified-Since Header") = secure {
    checkExamples(Seq(
      ("If-Modified-Since: Wed, 21 Oct 2015 07:28:00 GMT", `If-Modified-Since`(ZonedDateTime.parse("2015-10-21T07:28:00+00:00").toLocalDateTime), "If-Modified-Since: Wed, 21 Oct 2015 07:28:00 GMT")
    ))
  }

  property("If-None-Match Header") = secure {
    checkExamples(Seq(
      ("If-None-Match: *", `If-None-Match`(EntityTagRange.Any), "If-None-Match: *")
      , ("If-None-Match: \"bfc13a64729c4290ef5b2c2730249c88ca92d82d\""
        , `If-None-Match`(EntityTagRange.Range(List(EntityTag("bfc13a64729c4290ef5b2c2730249c88ca92d82d", weak = false))))
        , "If-None-Match: \"bfc13a64729c4290ef5b2c2730249c88ca92d82d\"")
      , ("If-None-Match: W/\"67ab43\", \"54ed21\", \"7892dd\""
        , `If-None-Match`(EntityTagRange.Range(List(EntityTag("67ab43", weak = true), EntityTag("54ed21", weak = false), EntityTag("7892dd", weak = false))))
        , "If-None-Match: W/\"67ab43\", \"54ed21\", \"7892dd\"")
    ))
  }


  property("Keep-Alive Header") = secure {
    checkExamples(Seq(
      ("Keep-Alive: timeout=5, max=1000", `Keep-Alive`(KeepAliveParams(5.seconds, 1000)), "Keep-Alive: timeout=5, max=1000")
    ))
  }


  property("Last-Modified Header") = secure {
    checkExamples(Seq(
      ("Last-Modified: Wed, 21 Oct 2015 07:28:00 GMT", `Last-Modified`(ZonedDateTime.parse("2015-10-21T07:28:00+00:00").toLocalDateTime), "Last-Modified: Wed, 21 Oct 2015 07:28:00 GMT")
    ))
  }


  property("Location Header") = secure {
    checkExamples(Seq(
      ("Location: /index.html", Location(LocationDefinition.Relative(Path / "index.html", Query.empty)), "Location: /index.html")
      , ("Location: index.html", Location(LocationDefinition.Relative(Path.relative("index.html"), Query.empty)), "Location: index.html")
      , ("Location: index.html?Q=A&Z=A%22", Location(LocationDefinition.Relative(Path.relative("index.html"), Query("Q", "A") :+ ("Z", "A\""))), "Location: index.html?Q=A&Z=A%22")
      , ("Location: http://www.spinoco.com/"
        , Location(LocationDefinition.Absolute(Uri(HttpScheme.HTTP, HostPort("www.spinoco.com", None), Path(initialSlash = true, trailingSlash = false, segments = Nil) , Query.empty)))
        , "Location: http://www.spinoco.com/")
      , ("Location: http://www.spinoco.com/index.html"
        , Location(LocationDefinition.Absolute(Uri(HttpScheme.HTTP, HostPort("www.spinoco.com", None), Path / "index.html", Query.empty)))
        , "Location: http://www.spinoco.com/index.html")
      , ("Location: http://www.spinoco.com:8080/index.html"
        , Location(LocationDefinition.Absolute(Uri(HttpScheme.HTTP, HostPort("www.spinoco.com", Some(8080)), Path / "index.html", Query.empty)))
        , "Location: http://www.spinoco.com:8080/index.html")
      , ("Location: http://www.spinoco.com:8080/index.html?Q=A"
        , Location(LocationDefinition.Absolute(Uri(HttpScheme.HTTP, HostPort("www.spinoco.com", Some(8080)), Path / "index.html", Query("Q","A"))))
        , "Location: http://www.spinoco.com:8080/index.html?Q=A")
    ))
  }


  property("Origin Header") = secure {
    checkExamples(Seq(
      ("Origin: https://developer.mozilla.org"
        , Origin(List(HttpOrigin.One("https", Some(HostPort("developer.mozilla.org", None)))))
        , "Origin: https://developer.mozilla.org")
      ,  ("Origin: file://"
        , Origin(List(HttpOrigin.One("file", None)))
        , "Origin: file://")
    ))
  }

  property("Pragma Header") = secure {
    checkExamples(Seq(
      ("Pragma: no-cache ", Pragma("no-cache"), "Pragma: no-cache")
    ))
  }

  property("Range Header") = secure {
    checkExamples(Seq(
      ("Range: bytes=0-100" , Range(ByteRange.Slice(0,100)) , "Range: bytes=0-100")
      , ("Range: bytes=100-" , Range(ByteRange.FromOffset(100)) , "Range: bytes=100-")
      , ("Range: bytes=-100" , Range(ByteRange.Suffix(100)) , "Range: bytes=-100")
    ))
  }

  property("Referer Header") = secure {
    checkExamples(Seq(
      ("Referer: http://127.0.0.1:9090/foo ", Referer(Uri.http("127.0.0.1", 9090, "/foo")), "Referer: http://127.0.0.1:9090/foo")
    ))
  }

  property("Sec-WebSocket-Accept Header") = secure {
    checkExamples(Seq(
      ("Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
        , `Sec-WebSocket-Accept`(ByteVector.fromHex("b37a4f2cc0624f1690f64606cf385945b2bec4ea").get)
        , "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
    ))
  }

  property("Sec-WebSocket-Extensions Header") = secure {
    checkExamples(Seq(
      ("Sec-WebSocket-Extensions: ext1, ext2"
        , `Sec-WebSocket-Extensions`(List("ext1", "ext2"))
        , "Sec-WebSocket-Extensions: ext1, ext2")
    ))
  }

  property("Sec-WebSocket-Key Header") = secure {
    checkExamples(Seq(
      ("Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=="
        , `Sec-WebSocket-Key`(ByteVector.fromHex("7468652073616d706c65206e6f6e6365").get)
        , "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==")
    ))
  }

  property("Sec-WebSocket-Protocol Header") = secure {
    checkExamples(Seq(
      ("Sec-WebSocket-Protocol: chat"
        , `Sec-WebSocket-Protocol`(List("chat"))
        , "Sec-WebSocket-Protocol: chat")
    ))
  }

  property("Sec-WebSocket-Version Header") = secure {
    checkExamples(Seq(
      ("Sec-WebSocket-Version: 13"
        , `Sec-WebSocket-Version`(13)
        , "Sec-WebSocket-Version: 13")
    ))
  }


  property("Server Header") = secure {
    checkExamples(Seq(
      ("Server: Apache ", Server(ServerProduct(List(ProductDescription("Apache", None)))), "Server: Apache")
      , ("Server: Apache/2.4.1 ", Server(ServerProduct(List(ProductDescription("Apache", Some("2.4.1"))))), "Server: Apache/2.4.1")
    ))
  }

  property("Set-Cookie Header") = secure {
    checkExamples(Seq(
      ("Set-Cookie: SESSID=298zf09hf012fh2; csrftoken=u32t4o3tb3gg43; _gat=1"
        , `Set-Cookie`(HttpCookie("SESSID", "298zf09hf012fh2", None, None, None, None, false, false, Map("csrftoken" -> "u32t4o3tb3gg43", "_gat" -> "1")))
        , "Set-Cookie: SESSID=298zf09hf012fh2; csrftoken=u32t4o3tb3gg43; _gat=1")
      , ("Set-Cookie: SESSID=298zf09hf012fh2; Max-Age=300"
        , `Set-Cookie`(HttpCookie("SESSID", "298zf09hf012fh2", None, Some(300.seconds), None, None, false, false, Map.empty))
        , "Set-Cookie: SESSID=298zf09hf012fh2; Max-Age=300")
      , ("Set-Cookie: SESSID=298zf09hf012fh2; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
        , `Set-Cookie`(HttpCookie("SESSID", "298zf09hf012fh2", Some(ZonedDateTime.parse("1994-11-06T08:49:37+00:00").toLocalDateTime), None, None, None, false, false, Map.empty))
        , "Set-Cookie: SESSID=298zf09hf012fh2; Expires=Sun, 6 Nov 1994 08:49:37 GMT")
      , ("Set-Cookie: SESSID=298zf09hf012fh2; Domain=that.domain.com"
        , `Set-Cookie`(HttpCookie("SESSID", "298zf09hf012fh2", None, None, Some("that.domain.com"), None, false, false, Map.empty))
        , "Set-Cookie: SESSID=298zf09hf012fh2; Domain=that.domain.com")
      , ("Set-Cookie: SESSID=298zf09hf012fh2; Path=/some/path"
        , `Set-Cookie`(HttpCookie("SESSID", "298zf09hf012fh2", None, None, None, Some("/some/path"), false, false, Map.empty))
        , "Set-Cookie: SESSID=298zf09hf012fh2; Path=/some/path")
      , ("Set-Cookie: SESSID=298zf09hf012fh2; secure"
        , `Set-Cookie`(HttpCookie("SESSID", "298zf09hf012fh2", None, None, None, None, secure = true, false, Map.empty))
        , "Set-Cookie: SESSID=298zf09hf012fh2; secure")
      , ("Set-Cookie: SESSID=298zf09hf012fh2; httponly"
        , `Set-Cookie`(HttpCookie("SESSID", "298zf09hf012fh2", None, None, None, None, false, httpOnly = true, Map.empty))
        , "Set-Cookie: SESSID=298zf09hf012fh2; httponly")
    ))
  }

  property("Transfer-Encoding Header") = secure {
    checkExamples(Seq(
      ("Transfer-Encoding: chunked" , `Transfer-Encoding`(List("chunked")) , "Transfer-Encoding: chunked")
    ))
  }

  property("Upgrade Header") = secure {
    checkExamples(Seq(
      ("Upgrade: websocket" , Upgrade(List(ProductDescription("websocket", None))) , "Upgrade: websocket")
    ))
  }

  property("Upgrade-Insecure-Requests Header") = secure {
    checkExamples(Seq(
      ("Upgrade-Insecure-Requests: 1 ", `Upgrade-Insecure-Requests`(1), "Upgrade-Insecure-Requests: 1")
    ))
  }

  property("User-Agent Header") = secure {
    checkExamples(Seq(
      ("User-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0"
        , `User-Agent`(AgentVersion("Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0"))
        , "User-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0")
    ))
  }


  property("WWW-Authenticate Header") = secure {
    checkExamples(Seq(
      ("WWW-Authenticate: Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\""
        , `WWW-Authenticate`(HttpChallenge(HttpChallengeScheme.Digest, List("realm" -> "testrealm@host.com", "qop" -> "auth,auth-int", "nonce" -> "dcd98b7102dd2f0e8b11d0f600bfb0c093", "opaque"->"5ccc069c403ebaf9f0171e9517f40e41")))
        , "WWW-Authenticate: Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093, opaque=5ccc069c403ebaf9f0171e9517f40e41")
      , ("WWW-Authenticate: OAuth \"Facebook Platform\" \"invalid_request\" \"Invalid appsecret_proof provided in the API argument\""
        , `WWW-Authenticate`(HttpChallenge(HttpChallengeScheme.OAuth, List("realm" -> "Facebook Platform", "error" -> "invalid_request", "error_description" -> "Invalid appsecret_proof provided in the API argument")))
        , "WWW-Authenticate: OAuth realm=\"Facebook Platform\", error=invalid_request, error_description=\"Invalid appsecret_proof provided in the API argument\"")
    ))
  }


  property("X-Forwarded-For Header") = secure {
    checkExamples(Seq(
      ("X-Forwarded-For: willi.wonka.com, www.spinoco.com, 1.1.2.2:80"
        , `X-Forwarded-For`(List(HostPort("willi.wonka.com", None), HostPort("www.spinoco.com", None), HostPort("1.1.2.2", Some(80))))
        , "X-Forwarded-For: willi.wonka.com, www.spinoco.com, 1.1.2.2:80")
    ))
  }

  property("X-Real-IP Header") = secure {
    checkExamples(Seq(
      ("X-Real-IP: 1.2.3.4" , `X-Real-IP`(HostPort("1.2.3.4", None)) , "X-Real-IP: 1.2.3.4")
    ))
  }


}
