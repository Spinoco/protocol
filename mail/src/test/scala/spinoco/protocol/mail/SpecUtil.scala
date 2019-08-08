package spinoco.protocol.mail

import org.scalacheck.Prop
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import org.scalacheck.Prop._

/**
  * Created by pach on 18/10/17.
  */
object SpecUtil {


  def verify[A](encoded: String, decoded: A)(implicit C: Codec[A]): Prop = {
    verify(encoded, decoded, encoded)
  }

  def verify[A](encoded: String, decoded: A, expect: String)(implicit C: Codec[A]): Prop = {
    ((C.decode(ByteVector.view(encoded.getBytes).bits) ?= Attempt.Successful(
      DecodeResult(decoded, BitVector.empty)
    )) :| s"Decoded value: $encoded") &&
      ((C.encode(decoded).map(_.bytes) ?= Attempt.Successful(ByteVector.view(expect.getBytes)))
        :| s"Encoded value: $decoded as: ${C.encode(decoded).map(_.bytes.decodeUtf8)}")
  }

  def verifyBytes[A](bytes: ByteVector, decoded: A, expect: String)(implicit C: Codec[A]): Prop = {
    ((C.decode(bytes.bits) ?= Attempt.Successful(
      DecodeResult(decoded, BitVector.empty)
    )) :| s"Decoded value: $bytes") &&
      ((C.encode(decoded).map(_.bytes) ?= Attempt.Successful(ByteVector.view(expect.getBytes)))
        :| s"Encoded value: $decoded as: ${C.encode(decoded).map(_.bytes.decodeUtf8)}")
  }

}
