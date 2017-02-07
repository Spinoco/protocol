//package spinoco.protocol.codec
//
//import org.scalacheck.Prop._
//import org.scalacheck.{Prop, Properties}
//import scodec.bits.BitVector
//import scodec.{Attempt, DecodeResult}
//import spinoco.protocol.http.codec.HttpHeaderCodec
//import spinoco.protocol.http.header._
//import spinoco.protocol.http.header.value._
//
///**
//  * Created by pach on 03/02/17.
//  */
//object TempSpec extends Properties("Temp") {
//
//
//  val codec = HttpHeaderCodec.codec(maxHeaderLength = Int.MaxValue)
//
//  def checkExamples(examples:Seq[(String, HttpHeader, String)]):Prop = {
//    examples.foldLeft(true:Prop) {
//      case (prop, (example, header, encoded)) =>
//        prop && (("Decode: " + example) |: (
//          codec.decode(BitVector.view(example.getBytes())) ?= Attempt.Successful(
//            DecodeResult(header, BitVector.empty)
//          )
//          )) && (("Encode: " + encoded) |: (
//          codec.encode(header).map(_.decodeAscii.fold(_.getMessage, identity)) ?= Attempt.Successful(encoded)
//          ))
//    }
//  }
//
//  property("Authorization  Header") = secure {
//    checkExamples(Seq(
//      ("Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
//        , Authorization(HttpCredentials.BasicHttpCredentials("Aladdin", "open sesame"))
//        , "Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
//      , ("Authorization: Bearer mF_9.B5f-4.1JqM"
//        , Authorization(HttpCredentials.OAuth2BearerToken("mF_9.B5f-4.1JqM"))
//        , "Authorization: Bearer mF_9.B5f-4.1JqM")
//      , ("Authorization: Digest username=\"Mufasa\",\n                 realm=\"testrealm@host.com\",\n                 nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\",\n                 uri=\"/dir/index.html\",\n                 qop=auth,\n                 nc=00000001,\n         cnonce=\"0a4f113b\",\n                 response=\"6629fae49393a05397450978507c4ef1\",\n                 opaque=\"5ccc069c403ebaf9f0171e9517f40e41\""
//        , Authorization(HttpCredentials.DigestHttpCredentials(Map(
//        "username" -> "Mufasa"
//        , "realm" -> "testrealm@host.com"
//        , "nonce" -> "dcd98b7102dd2f0e8b11d0f600bfb0c093"
//        , "uri" -> "/dir/index.html"
//        , "qop" -> "auth"
//        , "nc" -> "00000001"
//        , "cnonce" -> "0a4f113b"
//        , "response" -> "6629fae49393a05397450978507c4ef1"
//        , "opaque" -> "5ccc069c403ebaf9f0171e9517f40e41"
//      )))
//        , "Authorization: Digest nc=00000001, nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093, username=Mufasa, uri=\"/dir/index.html\", cnonce=0a4f113b, qop=auth, response=6629fae49393a05397450978507c4ef1, opaque=5ccc069c403ebaf9f0171e9517f40e41, realm=\"testrealm@host.com\"")
//    ))
//  }
//
//}
