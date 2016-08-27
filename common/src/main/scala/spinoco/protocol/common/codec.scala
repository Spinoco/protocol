package spinoco.protocol.common

import java.util.Date
import java.util.concurrent.TimeUnit

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._

import scala.concurrent.duration.{FiniteDuration, TimeUnit}


object codec {

  /**
    * Performs Xor operation of `codec` with `or`
    */
  final def xor[A](codec: Codec[A], or: BitVector): Codec[A] = {
    new Codec[A] {
      def decode(bits: BitVector): Attempt[DecodeResult[A]] =
        codec.decode(bits.xor(or.take(bits.length).padLeft(bits.length))).
          map { case DecodeResult(decoded, rest) => DecodeResult(decoded, bits.drop(bits.length - rest.length)) }
      def encode(value: A): Attempt[BitVector] = codec.encode(value).map(_.xor(or))

      def sizeBound: SizeBound = SizeBound.choice(List(codec.sizeBound, SizeBound.exact(or.size)))
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

  /** encodes duration in Ms for Int **/
  def durationIntMs(codec:Codec[Int]):Codec[FiniteDuration] =
    duration[Int](TimeUnit.MILLISECONDS, _.toInt)(codec)

  /** creates duration from specified units **/
  def duration[N](units:TimeUnit, mkN: Double => N)(codec:Codec[N])(implicit N: Numeric[N]):Codec[FiniteDuration] = {
    def decode(n:N):FiniteDuration =
      FiniteDuration(N.toLong(n), units)

    def encode(dur:FiniteDuration):N =
       mkN(dur.toUnit(units))

    codec.xmap(decode,encode)
  }

  /** encodes as ms since epoch **/
  val epochTimestamp:Codec[Date] = {
    int64.xmap(new Date(_), _.getTime)
  }




}
