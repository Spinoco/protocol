package spinoco.protocol.common

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._


object codec {

  /**
    * Performs Xor operation of `codec` with `or`
    */
  final def xor[A](codec: Codec[A], or: BitVector): Codec[A] = {
    new Codec[A] {
      override def decode(bits: BitVector): Attempt[DecodeResult[A]] =
        codec.decode(bits.xor(or.take(bits.length).padLeft(bits.length))).
          map { case DecodeResult(decoded, rest) => DecodeResult(decoded, bits.drop(bits.length - rest.length)) }
      override def encode(value: A): Attempt[BitVector] = codec.encode(value).map(_.xor(or))

      override def sizeBound: SizeBound = SizeBound.choice(List(codec.sizeBound, SizeBound.exact(or.size)))
    }.withToString(s"xor($codec ^ $or)")
  }


  /**
    * Applies predicate `f` to check whether `A` was decoded successfully, or may be encoded.
    * If `f` yields to String, then this signals failure
    */
  def predicate[A](c:Codec[A])(f: A => Option[String]):Codec[A] = {
    c.exmap(
      a => Attempt.fromErrOption(f(a).map(Err.apply), a)
    , a => Attempt.fromErrOption(f(a).map(Err.apply), a)
    )
  }

  /** int codec, that allows min/max bounds inclusive **/
  def intBounded(codec:Codec[Int])(min:Int, max:Int):Codec[Int] =
    predicate(uint16) { i =>
      if (i < min || i > max) Some(s"int is required to be within bounds [$min,$max] but is $i")
      else None
    }

  /** string codec, that allows min/max bounds on size inclusive **/
  def stringBounded(codec:Codec[String])(min:Int,max:Int):Codec[String] =
    predicate(codec) { s =>
      if (s.length < min || s.length > max) Some(s"string is required to be within bounds [$min,$max] but is (${s.length}): '$s'")
      else None
    }

}
