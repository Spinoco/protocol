package spinoco.protocol.common

import java.nio.charset.{Charset, StandardCharsets}

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector

/**
  * Terminator is used to find chunk of data, that may be used to decode chunks of bits terminated by `A`
  *
  */
trait Terminator[A] extends Codec[(BitVector, A)]


object Terminator {


  def constantString1(terminator: String, charset: Charset = StandardCharsets.UTF_8): Terminator[Unit] =
    constant1(BitVector.view(terminator.getBytes(charset)))

  def constantString(decoded: String, encoded: String, charset: Charset = StandardCharsets.UTF_8): Terminator[Unit] =
    constant(BitVector.view(decoded.getBytes(charset)), BitVector.view(encoded.getBytes(charset)))


  def constant1(terminator: BitVector): Terminator[Unit] =
    constant(terminator, terminator)

  /**
    * Constant terminator, that will encode terminator by `encoded` and will decode
    * chunks termianted by `decoded`
    * @return
    */
  def constant(decoded: BitVector, encoded: BitVector): Terminator[Unit] = {
    new Terminator[Unit] {
       def decode(bits: BitVector): Attempt[DecodeResult[(BitVector, Unit)]] = {
         val idx = bits.indexOfSlice(decoded)
         if (idx < 0) Attempt.failure(Err("Terminator not found"))
         else {
           val (h, t) = bits.splitAt(idx)
           Attempt.successful(DecodeResult((h, ()), t.drop(decoded.size)))
         }
       }

       def encode(value: (BitVector, Unit)): Attempt[BitVector] =
         Attempt.successful(value._1 ++ encoded)

       def sizeBound: SizeBound = SizeBound.unknown
    }
  }



}