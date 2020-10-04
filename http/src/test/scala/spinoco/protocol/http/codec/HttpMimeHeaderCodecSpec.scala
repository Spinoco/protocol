package spinoco.protocol.http.codec

import org.scalacheck.Properties
import org.scalacheck.Prop._
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import spinoco.protocol.http.mime.MIMEHeader
import spinoco.protocol.http.header._
import spinoco.protocol.mime._

object HttpMimeHeaderCodecSpec extends Properties("HttpMimeHeaderCodec"){


  property("decode") = secure {
    HttpMimeHeaderCodec.defaultCodec.decode(BitVector(
      Seq(
        "Content-Disposition: form-data; name=\"field1\""
        , "Content-Type: text/plain;charset=UTF-8"
        , "Content-Transfer-Encoding: quoted-printable"
      ).mkString("\r\n").getBytes()
    )) ?= Attempt.successful(DecodeResult(
      MIMEHeader(
        List(
          `Content-Disposition`(ContentDisposition(ContentDispositionType.IETFToken("form-data"), Map("name" -> "field1")))
          , `Content-Type`(ContentType.TextContent(MediaType.`text/plain`, Some(MIMECharset.`UTF-8`)))
          , `Content-Transfer-Encoding`(TransferEncoding.QuotedPrintable)
        )
      )
      , BitVector.empty))
  }

  property("encode") = secure {
    HttpMimeHeaderCodec.defaultCodec.encode(
      MIMEHeader(
        List(
          `Content-Disposition`(ContentDisposition(ContentDispositionType.IETFToken("form-data"), Map("name" -> "field1")))
          , `Content-Type`(ContentType.TextContent(MediaType.`text/plain`, Some(MIMECharset.`UTF-8`)))
          , `Content-Transfer-Encoding`(TransferEncoding.QuotedPrintable)
        )
      )
    ).map(_.decodeAscii) ?= Attempt.successful(Right(
      Seq(
        "Content-Disposition: form-data; name=field1"
        , "Content-Type: text/plain; charset=utf-8"
        , "Content-Transfer-Encoding: quoted-printable"
      ).mkString("\r\n")
    ))

  }

}
