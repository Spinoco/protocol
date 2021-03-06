package spinoco.protocol.mime

import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits.BitVector
import spinoco.protocol.mime.ContentType._

object CodecSpec extends Properties("CodecSpec") {

  def checkExamples[A](examples:Seq[(String, A, String)])(implicit codec: Codec[A]): Prop = {
    examples.foldLeft(true:Prop) {
      case (prop, (example, header, encoded)) =>
        prop && (("Decode: " + example) |: (
          codec.decode(BitVector.view(example.getBytes())) ?= Attempt.Successful(
            DecodeResult(header, BitVector.empty)
          )
          )) && (("Encode: " + encoded) |: (
          codec.encode(header).map(_.decodeAscii.fold(_.getMessage, identity).split(';').toSet) ?= Attempt.Successful(encoded.split(';').toSet)
          ))
    }

  }

  property("ContentType") = secure {
    implicit val codec:Codec[ContentType] = ContentType.codec

    checkExamples[ContentType](Seq(
      ("audio/ogg",BinaryContent(MediaType.`audio/ogg`, None), "audio/ogg")
      , ("Audio/OGG",BinaryContent(MediaType.`audio/ogg`, None), "audio/ogg")
      , ("application/json; charset=utf-8",BinaryContent(MediaType.`application/json`, Some(MIMECharset.`UTF-8`)), "application/json; charset=utf-8")
      , ("text/html; charset=utf-8",TextContent(MediaType.`text/html`, Some(MIMECharset.`UTF-8`)), "text/html; charset=utf-8")
      , ("multipart/form-data; boundary=something"
        , MultiPartContent(MediaType.`multipart/form-data`.copy(parameters = Map("boundary" -> "something")), charset = None)
        , """multipart/form-data; boundary="something""""
      )
      , ("""multipart/signed; protocol="application/pgp-signature"; micalg="pgp-sha1"; boundary="===============6480331919205975==""""
        , MultiPartContent(MediaType.`multipart/signed`.copy(parameters = Map("boundary" -> "===============6480331919205975==", "micalg" -> "pgp-sha1", "protocol" -> "application/pgp-signature")), charset = None)
        , """multipart/signed; protocol="application/pgp-signature"; micalg="pgp-sha1"; boundary="===============6480331919205975==""""
      )
      , ("""multipart/encrypted; protocol="application/pgp-signature"; boundary="===============6480331919205975==""""
        , MultiPartContent(MediaType.`multipart/encrypted`.copy(parameters = Map("boundary" -> "===============6480331919205975==", "protocol" -> "application/pgp-signature")), charset = None)
        , """multipart/encrypted; protocol="application/pgp-signature"; boundary="===============6480331919205975==""""
      )
      , ("""multipart/signed; micalg="pgp-sha1"; protocol="application/pgp-signature"; boundary="===============7108545632574332286==""""
        , MultiPartContent(MediaType.`multipart/signed`.copy(parameters = Map("boundary" -> "===============7108545632574332286==", "protocol" -> "application/pgp-signature", "micalg" -> "pgp-sha1")), charset = None)
        , """multipart/signed; protocol="application/pgp-signature"; micalg="pgp-sha1"; boundary="===============7108545632574332286==""""
      )
      , ("""multipart/signed; boundary="Apple-Mail=_A5434659-C987-446B-BA42-479AB306275E"; protocol="application/pkcs7-signature"; micalg=sha1"""
        , MultiPartContent(MediaType.`multipart/signed`.copy(parameters = Map("boundary" -> "Apple-Mail=_A5434659-C987-446B-BA42-479AB306275E", "protocol" -> "application/pkcs7-signature", "micalg" -> "sha1")), charset = None)
        , """multipart/signed; protocol="application/pkcs7-signature"; micalg="sha1"; boundary="Apple-Mail=_A5434659-C987-446B-BA42-479AB306275E""""
      ), ( """multipart/mixed; charset=utf-8; boundary="4=_k47S1kfn7l1CnpwB9LRjjHMwBfroxwJ""""
        , MultiPartContent(MediaType.`multipart/mixed`.copy(parameters = Map("boundary" -> "4=_k47S1kfn7l1CnpwB9LRjjHMwBfroxwJ")), charset = Some(MIMECharset.`UTF-8`))
        , """multipart/mixed; charset=utf-8; boundary="4=_k47S1kfn7l1CnpwB9LRjjHMwBfroxwJ""""
      )
    ))
  }

  property("Content-Disposition") = secure {
    implicit val codec:Codec[ContentDisposition] = ContentDisposition.emailCodec

    checkExamples(Seq(
      ("form-data", ContentDisposition(ContentDispositionType.IETFToken("form-data"), Map.empty), "form-data")
      , ("attachment;\r\n filename=\"filename.jpg\""
        , ContentDisposition(ContentDispositionType.Attachment, Map("filename" -> "filename.jpg"))
        , "attachment;\r\n filename=\"filename.jpg\""
      )
      , ("form-data; name=\"fieldName\";\r\n filename=\"filename.jpg\""
        , ContentDisposition(ContentDispositionType.IETFToken("form-data"), Map("name" -> "fieldName", "filename" -> "filename.jpg"))
        , "form-data;\r\n name=fieldName;\r\n filename=\"filename.jpg\""
      )
    ))
  }


  property("Content-Disposition with extension") = secure {
    implicit val codec:Codec[ContentDisposition] = ContentDisposition.emailCodec

    checkExamples(Seq(
      ("message/external-body; access-type=URL;\n      URL*0=\"ftp://\";\n      URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\""
        , ContentDisposition(ContentDispositionType.IETFToken("message/external-body"), Map("access-type" -> "URL", "URL" -> "ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar"))
        , "message/external-body;\r\n access-type=URL;\r\n URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\""
      ),
      ("attachment;\r\n filename*=utf-8''%C4%9B%C4%9B%C5%99%C5%BE%C3%BD%C5%BE%2E%6A%70%67"
        , ContentDisposition(ContentDispositionType.Attachment, Map("filename" -> "ěěřžýž.jpg"))
        , "attachment;\r\n filename*=utf-8''%C4%9B%C4%9B%C5%99%C5%BE%C3%BD%C5%BE%2E%6A%70%67"
      )
    ))
  }

}
