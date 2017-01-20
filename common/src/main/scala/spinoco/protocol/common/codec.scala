package spinoco.protocol.common

import java.nio.charset.StandardCharsets
import java.util.Date
import java.util.concurrent.TimeUnit

import scodec.bits.{BitVector, ByteVector}
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

  /**
    * Decodes bytes util `f` holds. Encodes as identity
    */
  def bytesUntil(f: Byte => Boolean):Codec[ByteVector] = new Codec[ByteVector] {
    def decode(bits: BitVector): Attempt[DecodeResult[ByteVector]] = {
      val h = bits.bytes.takeWhile(f)
      val t = bits.drop(h.size*8)
      Attempt.successful(DecodeResult(h, t))
    }
    def encode(value: ByteVector): Attempt[BitVector] = Attempt.successful(value.bits)
    def sizeBound: SizeBound = SizeBound.unknown
  }





  /**
    * A codec that takes all bytes until `discriminator` is found. Then `codec` is applied to get `A`
    * Remainder AFTER discriminator is returned
    */
  def takeWhile[A](
    discriminator: ByteVector
    , encodingDiscriminator: ByteVector
    , codec:Codec[A]
    , maxLength: Int = Int.MaxValue
  ):Codec[A] = new Codec[A] {
    def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
      bits.bytes.take(maxLength).indexOfSlice(discriminator) match {
        case -1 => Attempt.failure(Err(s"Bytes are not terminated before byte at $maxLength by pattern $discriminator"))
        case index =>
          val (result, remainder) = bits.bytes.splitAt(index)
          codec.decode(result.bits).map(_.copy(remainder = remainder.drop(discriminator.size).bits))
      }
    }
    def encode(value: A): Attempt[BitVector] =
      codec.encode(value).map { _ ++ encodingDiscriminator.bits }


    def sizeBound: SizeBound = SizeBound.unknown
  }

  /**
    * Decodes bytes that are terminated by supplied byte, and then applies codec on bytes decoded
    * differs from `codec ~ delimiter` so the delimiter is scanned first and then `codec` is applied.
    *
    * The delimiter is not part of any remainder returned when decoding.
    *
    * instead using delimiter to encode, encDelimiter is used.
    */
  def terminatedBy[A](delimiter: ByteVector, encDelimiter: ByteVector, codec:Codec[A]):Codec[A] = {
    new Codec[A] {
      def sizeBound: SizeBound = SizeBound.unknown
      def encode(value: A): Attempt[BitVector] = {
        codec.encode(value).map { _ ++ encDelimiter.bits }
      }
      def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
        bits.bytes.indexOfSlice(delimiter) match {
          case -1 => codec.decode(bits)
          case idx =>
            val (h, t) = bits.bytes.splitAt(idx)
            codec.decode(h.bits).map { _.mapRemainder { _ => t.drop(delimiter.size).bits } }
        }
      }
    }
  }

  private def fromAsciiString[A](f: String => A, g: A => String):Codec[A] = {
    string(StandardCharsets.US_ASCII).exmap(
      s => try { Attempt.successful(f(s.trim.toLowerCase)) } catch { case t: Throwable => Attempt.failure(Err(s"Invalid format : $s : ${t.getMessage}")) }
      , b => Attempt.successful(g(b))
    )
  }

  lazy val boolAsString: Codec[Boolean] =
    fromAsciiString[Boolean](_.toBoolean, _.toString).withToString("boolAsString")


  /**
    * Float encoded as string value
    */
  lazy val floatAsString: Codec[Float] =
    fromAsciiString[Float](_.toFloat, _.toString).withToString("floatAsString")

  /**
    * Int encoded as string value
    */
  lazy val intAsString: Codec[Int] =
    fromAsciiString[Int](_.toInt, _.toString).withToString("intAsString")

  /**
    * Int encoded as string value
    */
  lazy val longAsString: Codec[Long] =
    fromAsciiString[Long](_.toLong, _.toString).withToString("longAsString")


  lazy val bytesWsRemoved: Codec[ByteVector] = {
    def stripWs(bs:ByteVector):ByteVector = {
      bs.dropWhile { _.toChar.isWhitespace }
      .reverse.dropWhile { _.toChar.isWhitespace }
      .reverse
    }

    bytes.xmap(stripWs,stripWs)
  }

  lazy val bitsWsRemoved: Codec[BitVector] =
    bytesWsRemoved.xmap(_.bits,_.bytes)



  implicit class ByteVectorCodecSyntax(val self: Codec[ByteVector]) extends AnyVal {

    def codedAs[A](aCodec: Codec[A]):Codec[A] = new Codec[A] {
      def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
        self.decode(bits).flatMap { case DecodeResult(bv, rem) =>
          aCodec.decodeValue(bv.bits).map { a => DecodeResult(a, rem) }
        }
      }
      def encode(value: A): Attempt[BitVector] = {
        aCodec.encode(value).flatMap { bits =>
          self.encode(bits.bytes)
        }
      }
      def sizeBound: SizeBound = self.sizeBound
    }

  }


}
