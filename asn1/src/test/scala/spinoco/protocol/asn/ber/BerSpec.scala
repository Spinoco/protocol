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
    verify(Option(120l), BitVector.fromInt(0x78, 8))(length)
  }

  property("length.128") = protect {
    verify(Option(128l), BitVector.fromInt(0x8180, 16))(length)
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

}
