package spinoco.protocol.asn.ber

import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}
import scodec.Attempt.{Failure, Successful}
import scodec.{Attempt, Codec, Err}
import scodec.bits.BitVector

object BerSpec extends Properties("BER"){

  def verify[A](verify: A, shouldBe: BitVector)(C: Codec[A]): Prop = {
    (C.encode(verify) match {
      case Failure(err) => Prop.falsified :| err.messageWithContext
      case Successful(a) => a ?= shouldBe
    }) && (C.decode(shouldBe) match {
      case Failure(err) => Prop.falsified :| err.messageWithContext
      case Successful(a) => a.value ?= verify
    })
  }

  property("length.less.127") = protect {
    verify(Option(120L), BitVector.fromInt(0x78, 8))(length)
  }

  property("length.128") = protect {
    verify(Option(128L), BitVector.fromInt(0x8180, 16))(length)
  }

  property("length.3383") = protect {
    // Tests proper encoding of "D37" <- partial octet
    verify(Option(3383L), BitVector.fromInt(0x820D37 , 24))(length)
  }

  property("length.long-max") = protect {
    verify(Option(Long.MaxValue), BitVector.fromInt(0x88, 8) ++ BitVector.fromLong(Long.MaxValue))(length)
  }

  property("identifier.universal.primitive") = protect {
    verify(Identifier(BerClass.Universal, false, 2), BitVector.fromInt(0x02, 8))(identifier)
  }

  property("identifier.universal.constructed") = protect {
    verify(Identifier(BerClass.Universal, true, 2), BitVector.fromInt(0x22, 8))(identifier)
  }

  property("identifier.application.primitive") = protect {
    verify(Identifier(BerClass.Application, false, 2), BitVector.fromInt(0x42, 8))(identifier)
  }

  property("identifier.application.constructed") = protect {
    verify(Identifier(BerClass.Application, true, 2), BitVector.fromInt(0x62, 8))(identifier)
  }

  property("identifier.context.primitive") = protect {
    verify(Identifier(BerClass.Context, false, 2), BitVector.fromInt(0x82, 8))(identifier)
  }

  property("identifier.context.constructed") = protect {
    verify(Identifier(BerClass.Context, true, 2), BitVector.fromInt(0xA2, 8))(identifier)
  }

  property("identifier.private.primitive") = protect {
    verify(Identifier(BerClass.Private, false, 2), BitVector.fromInt(0xC2, 8))(identifier)
  }

  property("identifier.private.constructed") = protect {
    verify(Identifier(BerClass.Private, true, 2), BitVector.fromInt(0xE2, 8))(identifier)
  }

  property("finite-length.insufficient") = protect {
    val codec = finiteLength(scodec.codecs.provide(()))

    codec.decode(BitVector.fromValidHex("8180")) ?= Attempt.failure(Err.insufficientBits(128 * 8, 0))
  }

  property("identifier.insufficient") = protect {
    identifier.decode(BitVector.fromValidHex("A")) ?= Attempt.failure(Err.insufficientBits(8, 4))
  }

  property("integer.encode") = protect {
    (compliment2Stripped.encode(64).require ?= BitVector.fromValidBin("01000000")) :| "Positive 64" &&
    (compliment2Stripped.encode(-64).require ?= BitVector.fromValidBin("11000000")) :| "Negative 64" &&
    (compliment2Stripped.encode(256).require ?= BitVector.fromValidBin("0000000100000000")) :| "Positive 256" &&
    (compliment2Stripped.encode(-256).require ?= BitVector.fromValidBin("1111111100000000")) :| "Negative 256"
  }

  property("integer.decode") = protect {
    (compliment2Stripped.decode(BitVector.fromValidBin("01000000")).require.value ?= 64) :| "Positive 64" &&
    (compliment2Stripped.decode(BitVector.fromValidBin("11000000")).require.value ?= -64) :| "Negative 64" &&
    (compliment2Stripped.decode(BitVector.fromValidBin("0000000100000000")).require.value ?= 256) :| "Positive 256" &&
    (compliment2Stripped.decode(BitVector.fromValidBin("1111111100000000")).require.value ?= -256) :| "Negative 256"&&
    (compliment2Stripped.decode(BitVector.fromValidBin("0000000000110010")).require.value ?= 50) :| "Padded 50"
  }

}
