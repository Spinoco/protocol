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
          codec.encode(header).map(_.decodeAscii.fold(_.getMessage, identity)) ?= Attempt.Successful(encoded)
          ))
    }

  }

  property("ContentType") = secure {
    implicit val codec:Codec[ContentType] = ContentType.codec

    checkExamples[ContentType](Seq(
      ("audio/ogg",BinaryContent(MediaType.`audio/ogg`), "audio/ogg")
      , ("Audio/OGG",BinaryContent(MediaType.`audio/ogg`), "audio/ogg")
      , ("text/html; charset=utf-8",TextContent(MediaType.`text/html`, Some(MIMECharset.`UTF-8`)), "text/html; charset=utf-8")
      , ("multipart/form-data; boundary=something"
        , MultiPartContent(MediaType.`multipart/form-data`.copy(parameters = Map("boundary" -> "something")))
        , "multipart/form-data; boundary=something"
      )
    ))
  }

  property("Content-Disposition") = secure {
    implicit val codec:Codec[ContentDisposition] = ContentDisposition.codec

    checkExamples(Seq(
      ("form-data", ContentDisposition("form-data", Map.empty), "form-data")
      , ("attachment; filename=\"filename.jpg\""
        , ContentDisposition("attachment", Map("filename" -> "filename.jpg"))
        , "attachment; filename=\"filename.jpg\""
      )
      , ("form-data; name=\"fieldName\";\n filename=\"filename.jpg\""
        , ContentDisposition("form-data", Map("name" -> "fieldName", "filename" -> "filename.jpg"))
        , "form-data; name=fieldName; filename=\"filename.jpg\""
      )
    ))
  }

}
