package spinoco.protocol.common

import java.nio.charset.StandardCharsets
import java.util.Date
import java.util.concurrent.TimeUnit

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._
import shapeless.tag
import shapeless.tag.@@

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
  def guard[A](c:Codec[A])(f: A => Option[Err]):Codec[A] = {
    c.exmap(
      a => Attempt.fromErrOption(f(a), a)
    , a => Attempt.fromErrOption(f(a), a)
    )
  }

  /** int codec, that allows min/max bounds inclusive **/
  def intBounded(codec:Codec[Int])(min:Int, max:Int):Codec[Int] =
    guard(uint16) { i =>
      if (i < min || i > max) Some(Err(s"int is required to be within bounds [$min,$max] but is $i"))
      else None
    }

  /** string codec, that allows min/max bounds on size inclusive **/
  def stringBounded(codec:Codec[String])(min:Int,max:Int):Codec[String] =
    guard(codec) { s =>
      if (s.length < min || s.length > max) Some(Err(s"string is required to be within bounds [$min,$max] but is (${s.length}): '$s'"))
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
    , codec: Codec[A]
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

  /** tags value `A` with `T` **/
  def tagged[A, T](codec: Codec[A]):Codec[A @@ T] =
    codec.xmap(tag[T](_), a => a)



  /** takes bytes until `f` holds, then decodes via `codec` **/
  def takeWhile[A](codec: Codec[A])(f: Byte => Boolean):Codec[A] = {
    new Codec[A] {
      def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
        val toDecode = bits.bytes.takeWhile(f)
        codec.decode(toDecode.bits).map { _.mapRemainder(_ ++ bits.drop(toDecode.size * 8)) }
      }

      def encode(value: A): Attempt[BitVector] = codec.encode(value)

      def sizeBound: SizeBound = SizeBound.unknown
    }
  }

  /** takes bytes until char or chars are encountered **/
  def takeWhileChar[A](codec: Codec[A])(char:Char, chars: Char*):Codec[A] = {
    if (chars.isEmpty) {
      val b = char.toByte
      takeWhile(codec)(_ != b)
    } else {
      val bs = (char.toByte +: chars.map(_.toByte)).toSet
      takeWhile(codec)(b => ! bs.contains(b))
    }

  }

  /** codec that decodes codec, until EOL signature is found. EOL is defined as crlf or lf only**/
  def untilEOL[A](codec: Codec[A], encodeNewLine: BitVector = BitVector.view("\r\n".getBytes)): Codec[A] = {
    new Codec[A] {
      val cr: Byte = '\r'
      val lf: Byte = '\n'
      def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
        val untilEOL = bits.bytes.takeWhile(b => !((b == cr) || (b == lf)))
        val bsSize = untilEOL.size*8
        if (bsSize == bits.size) codec.decode(untilEOL.bits)
        else {
          val rem = bits.drop(bsSize)

          if (rem.getByte(0) == cr && rem.getByte(1) == lf) codec.decode(untilEOL.bits).map { _.mapRemainder(_ ++ rem.drop(16)) }
          else if (rem.getByte(0) == lf) codec.decode(untilEOL.bits).map { _.mapRemainder(_ ++ rem.drop(8)) }
          else Attempt.failure(Err(s"End of line must be terminated with \\n or \\r, but is with ${rem.take(16).decodeUtf8}"))
        }
      }
      def encode(value: A): Attempt[BitVector] = codec.encode(value).map { _ ++ encodeNewLine }
      def sizeBound: SizeBound = SizeBound.unknown
}
  }

  /** codec that takes until any whitespace and then this applies supplied codec **/
  def untilWs[A](codec: Codec[A]):Codec[A] =
    takeWhile(codec)(! _.toChar.isWhitespace)


  /** drops while `f` holds. Then when encoding uses `encodeAs` **/
  def dropWhile(encodeAs: BitVector)(f: Byte => Boolean):Codec[Unit] = {
    new Codec[Unit] {
      def decode(bits: BitVector): Attempt[DecodeResult[Unit]] =
        Attempt.successful(DecodeResult((),bits.bytes.dropWhile(f).bits))
      def encode(value: Unit): Attempt[BitVector] = Attempt.successful(encodeAs)
      def sizeBound: SizeBound = SizeBound.unknown
    }
  }

  /** like Recover codec, but with fixed encode **/
  def recover2(codec:Codec[Unit]): Codec[Boolean] = new Codec[Boolean] {
    def encode(value: Boolean): Attempt[BitVector] =
      if (!value) Attempt.successful(BitVector.empty)
      else codec.encode(())
    def sizeBound: SizeBound = codec.sizeBound
    def decode(bits: BitVector): Attempt[DecodeResult[Boolean]] =
      codec.decode(bits).map { _.map { _ => true } }
      .recover { case _ => DecodeResult(false, bits) }
  }

  /** correct version of lookahead that won't encode `codec` when encoding **/
  def lookahead2(codec:Codec[Unit]):Codec[Boolean] = new Codec[Boolean] {
    def encode(value: Boolean): Attempt[BitVector] = Attempt.successful(BitVector.empty)
    def sizeBound: SizeBound = SizeBound.unknown
    def decode(bits: BitVector): Attempt[DecodeResult[Boolean]] =
      codec.decode(bits).map { _.map { _ => true }.mapRemainder(_ => bits) }
      .recover { case _ => DecodeResult(false, bits) }
  }



  def constantString(s:String):Codec[Unit] =
    constant(BitVector.view(s.getBytes))

  def stringEnumerated(discriminator: Codec[String], enumeration: Enumeration) =
    mappedEnum(discriminator, enumeration.values.map(e => e -> e.toString).toMap)


  /**
    * When decoding takes up bytes if open and close character are found. if there are multiple
    * open characters found, this decodes until matching close characters are found.
    * When encoding wraps the result of codec to open and close characters
    */
  def enclosedBy[A](open: Char, close: Char)(codec:Codec[A]):Codec[A] = {
    new Codec[A] {
      val p = open.toByte
      val s = close.toByte
      val prefix = BitVector(p)
      val suffix = BitVector(s)
      def sizeBound: SizeBound = SizeBound.unknown
      def encode(value: A): Attempt[BitVector] = codec.encode(value).map { bs => prefix ++ bs ++ suffix }
      def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
        if (bits.size < 8) Attempt.failure(Err(s"Expected $open, but no character is supplied: $bits"))
        else if (bits.getByte(0) != p) Attempt.failure(Err(s"Expected $open, but got ${bits.getByte(0).toChar}"))
        else {
          val bs = bits.bytes
          var open = 0
          val chunk = bs.takeWhile { b =>
            if (b == p) { open = open + 1; true }
            else if (b != s) true
            else {
              if (open == 1) false
              else { open = open - 1; true }
            }
          }

          if (chunk.size == bs.size) Attempt.failure(Err(s"Expected $open matcher by $close, but no matching pairs were found"))
          else {
            lazy val rem = bs.drop(chunk.size + 1) // drop close byte
            codec.decode(chunk.tail.bits).map { // drop the open byte
              _.mapRemainder { _ => rem.bits }
            }
          }
        }
      }
    }
  }

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

  implicit class UnitCodecSyntax(val self: Codec[Unit]) extends AnyVal {
    def decodeAs[A](a: A):Codec[A] = self.xmap(_ => a, _ => ())
  }




}
