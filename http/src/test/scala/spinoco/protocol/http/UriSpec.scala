package spinoco.protocol.http

import org.scalacheck.Properties
import org.scalacheck.Prop._
import scodec.Attempt
import scodec.bits.BitVector


object UriSpec extends Properties("Uri") {


  property("decode.encode") = secure {

    val examples = Seq(
      ("http://www.spinoco.com/"
      , Uri(HttpScheme.HTTP, HostPort("www.spinoco.com", None), Uri.Path.Root, Uri.Query.empty)
      , "http://www.spinoco.com/"
      )
      , ("https://www.spinoco.com/"
        , Uri(HttpScheme.HTTPS, HostPort("www.spinoco.com", None), Uri.Path.Root, Uri.Query.empty)
        , "https://www.spinoco.com/"
      )
      , ("http://www.spinoco.com:8080/"
        , Uri(HttpScheme.HTTP, HostPort("www.spinoco.com", Some(8080)), Uri.Path.Root, Uri.Query.empty)
        , "http://www.spinoco.com:8080/"
      )
      , ("http://www.spinoco.com:8080/that/path"
        , Uri(HttpScheme.HTTP, HostPort("www.spinoco.com", Some(8080)), Uri.Path / "that" / "path", Uri.Query.empty)
        , "http://www.spinoco.com:8080/that/path"
      )
      , ("http://www.spinoco.com:8080/that/path/"
        , Uri(HttpScheme.HTTP, HostPort("www.spinoco.com", Some(8080)), Uri.Path / "that" / "path" /, Uri.Query.empty)
        , "http://www.spinoco.com:8080/that/path/"
      )
      , ("http://127.0.0.1:8080/"
        , Uri(HttpScheme.HTTP, HostPort("127.0.0.1", Some(8080)), Uri.Path.Root, Uri.Query.empty)
        , "http://127.0.0.1:8080/"
      )
    )

    examples.foldLeft(proved) {
      case (p, (in, example, exampleEncoded)) =>
        (p
          && (s"DECODE: $in" |: ( Uri.codec.decodeValue(BitVector.view(in.getBytes)) ?= Attempt.Successful(example) ) )
          && (s"ENCODE: $in" |: (Uri.codec.encode(example).map(_.decodeUtf8) ?= Attempt.Successful(Right(exampleEncoded))))
          )
    }

  }


}
