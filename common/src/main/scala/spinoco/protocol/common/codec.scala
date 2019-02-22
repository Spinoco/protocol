package spinoco.protocol.common

import java.nio.charset.{Charset, StandardCharsets}
import java.util.Date
import java.util.concurrent.TimeUnit

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._
import shapeless.tag
import shapeless.tag.@@

import scala.annotation.tailrec
import scala.concurrent.duration.{FiniteDuration, TimeUnit}
import util.attemptFromEither
import util.attempt

import scala.collection.GenTraversable

object codec {



  /** codec that ignores any Whitespace when decoding **/
  val ignoreWS: Codec[Unit] = ignoreBytes(_.toChar.isWhitespace)

  /** codec that encodes/decodes to space **/
  val SPACE: Codec[Unit] = constantString1(" ")


  val asciiToken = token(ascii)

  val utf8Token = token(utf8)


  val quotedAsciiToken: Codec[String] = {
    constantString1("\"") ~> takeWhile(asciiToken)(_ != '"') <~ constantString1("\"")
  }

  /**
    * Decodes to token(string) eventually terminated by `terminator`. Terminator is not customed by this codec.. encodes to codec.
    * Terminator must be Ascii Char
    * @param codec          codec to terminate the value
    * @param terminator     tzerminator
    * @return
    */
  def token(codec: Codec[String], terminator: Char = ' '): Codec[String] = {
    new Codec[String] {
      def decode(bits: BitVector) = {
        val toDecode = bits.bytes.takeWhile(_ != terminator).bits
        codec.decode(toDecode) flatMap { case DecodeResult(s, rem) =>
          if (rem.nonEmpty) Attempt.failure(Err(s"Failed to decode token, still characters remaining: ${rem.bytes}"))
          else Attempt.successful(DecodeResult(s, bits.drop(toDecode.size)))
        }
      }

      def encode(value: String) = {
        if (value.exists(_ == terminator)) Attempt.failure(Err(s"Token may not contain termination character [$terminator]"))
        else codec.encode(value)
      }

      def sizeBound = codec.sizeBound
    }
  }

  /** converts string to bits, given supplied encoding **/
  def bitsOfString(s: String, chs: Charset = StandardCharsets.UTF_8):BitVector =
    BitVector.view(s.getBytes(chs))


  /** a codec that will ignore when decoding all bytes until `f` holds, On encoding will encode to empty **/
  def ignoreBytes(f: Byte => Boolean): Codec[Unit] = {
    val empty = Attempt.successful(BitVector.empty)
    new Codec[Unit] {
      def decode(bits: BitVector): Attempt[DecodeResult[Unit]] = {
        val bytes = bits.bytes.dropWhile(f)
        Attempt.successful(DecodeResult((), bytes.bits))
      }

      def encode(value: Unit): Attempt[BitVector] = empty

      val sizeBound: SizeBound = SizeBound.unknown
    }
  }



  /**
    * A generic codec to encode quoted string.
    *
    * Quoted string starts with Ascii `quote` char and continues with any character, compatible with charset.
    * Characters may be escaped by `escape` and correct character. Any `quote` char must be escaped.
    *
    * @param charset      Charset of the string. Must be ASCII backward compatible, not using first 7 bits of
    *                     each byte for the characters except the ascii ones.
    * @param quote        Quote enclosing the string. Must be ascii character
    * @param escape       Escape char, must be ascii.
    * @return
    */
  def quotedString(charset: Charset, quote: Char = '"', escape: Char = '\\'): Codec[String] = {
    val quoteByte = quote.toByte
    val escapeByte = escape.toByte
    val quoteString = quoteByte.toString
    val quoteStringEscaped = quoteString + "\""
    val quoteBits = BitVector.view(Array(quoteByte))

    new Codec[String] {
      def decode(bits: BitVector) = {
        if (!bits.startsWith(quoteBits)) Attempt.failure(Err(s"Quoted string does not start with $quote"))
        else {
          @tailrec
          def go(rem: ByteVector, acc: ByteVector): Attempt[(BitVector, BitVector)] = {
            val clean = rem.takeWhile { b => b != quoteByte && b!= escapeByte }
            val next = rem.drop(clean.size)
            next.headOption match {
              case Some(h) =>
                if (h != escapeByte) Attempt.successful(((acc ++ clean).bits, next.tail.bits))
                else go(next.drop(2), acc ++ clean ++ next.drop(1).take(1))

              case None => Attempt.failure(Err(s"Unterminated string constant, required termination with $quote"))
            }
          }

          go(bits.bytes.drop(1), ByteVector.empty).flatMap { case (stringBits, rem) =>
            attempt { charset.decode(stringBits.bytes.toByteBuffer) } map { s => DecodeResult(s.toString, rem) }
          }
        }
      }

      def encode(value: String) = {
        Attempt.successful {
          quoteBits ++
          BitVector.view(value.replace(quoteString, quoteStringEscaped).getBytes(charset)) ++
          quoteBits
        }
      }

      val sizeBound = SizeBound.unknown
    }
  }

  /** codec for ascii strings that may be quoted **/
  val quotedAsciiString: Codec[String] = quotedString(StandardCharsets.US_ASCII)
  val quotedUTF8String: Codec[String] = quotedString(StandardCharsets.UTF_8)

  val defaultQuotableChars: Set[Char] = "()<>@.,;:\\/[]?={} \t\"\'".toSet

  // Token definition as in https://tools.ietf.org/html/rfc2616#section-2.2
  val defaultHttpQuotableChars: Set[Char] = "()<>@,;:\\/[]?={} \t\"\'".toSet

  /**
    * Decodes string from eventually quoted string.
    * If the decoded string was not quoted and contains chars within `quotableChars` the encoding will fail
    *
    *
    * @param quotableChars    Chars that must be quoted
    * @param quote            A quoting char
    * @param escape           An escape char used to escape quote and eventually other chars.
    * @param charset          Charset
    * @return
    */
  def eventuallyQuotedString(quotableChars: Set[Char], quote: Char = '"', escape: Char = '\\', charset: Charset = StandardCharsets.UTF_8): Codec[String] = {
    val quoted = quotedString(charset, quote, escape)
    val nonQuoted = string(charset).narrow(s => {
      if (s.exists(quotableChars.contains)) Attempt.failure(Err(s"String must nor contain any of $quotableChars when not quoted by [$quote]"))
      else Attempt.successful(s)
    } , identity[String])
    new Codec[String] {

      def decode(bits: BitVector) =
        quoted.decode(bits) orElse nonQuoted.decode(bits)

      def encode(value: String) = {
        if (value.exists(quotableChars.contains)) quoted.encode(value.replace("\"", "\\\""))
        else nonQuoted.encode(value)
      }

      def sizeBound = SizeBound.unknown
    }
  }


  val eventuallyQuotedAsciiString: Codec[String] = eventuallyQuotedString(defaultQuotableChars, charset = StandardCharsets.US_ASCII)

  // To be used to decode (token | quoted-string) in http codecs
  val httpMaybeQuotedUTF8String: Codec[String] = eventuallyQuotedString(defaultHttpQuotableChars, charset = StandardCharsets.UTF_8)


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
    guard(codec) { i =>
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

  lazy val digits: Codec[Int] =
    takeWhile(ascii)(b => ('0'.toByte <= b) && (b <= '9'.toByte)).exmap({ str =>
      try { Attempt.successful(str.trim.toInt)} catch { case t: Throwable => Attempt.failure(Err(s"Invalid format : $str : ${t.getMessage}")) }
    }, { int =>
      Attempt.successful(int.toString)
    })

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




  /** codec that encodes and deoces to supplied string **/
  def constantString1(s: String): Codec[Unit] =
    constant(BitVector.view(s.getBytes))



  /** like `constantString1` but decodes when matches case insensitive. Works fro ascii only. **/
  def constantString1CaseInsensitive(s: String): Codec[Unit] = {
    new Codec[Unit] {
      val sBits = BitVector.view(s.getBytes())
      val sizeBound = SizeBound.exact(s.length)

      def encode(value: Unit) = Attempt.successful(sBits)

      def decode(bits: BitVector) = {
        val (h, t) = bits.splitAt(sBits.size)
        if (h == sBits) Attempt.successful(DecodeResult((), t))
        else {
          attemptFromEither(h.decodeAscii) flatMap { s0 =>
            if (s0.equalsIgnoreCase(s)) Attempt.successful(DecodeResult((), t))
            else Attempt.failure(Err(s"Expected $s (case insensitive) but got $s0"))
          }
        }
      }
    }
  }

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

  /**
    * A codec, that on decoding will first decode via `terminator` and when that will be successful,
    * will take all bits returned by `terminator` and passes them to be decoded by `codec`.
    *
    * On encoding, this will encode `A` followed by terminator.
    *
    * @param codec          Codec that encodes/decodes `A`
    * @param terminator     Terminator that returns sequence of data for `codec`
    */
  def terminated[A](codec: Codec[A], terminator: Terminator[Unit]): Codec[A] = {

    new Codec[A] {
      val sizeBound = SizeBound.unknown

      def decode(bits: BitVector) = {
        terminator.decode(bits) flatMap { case DecodeResult((aBits, _), rem) =>
          codec.decode(aBits) flatMap { case DecodeResult(a, remA) =>
            if (remA.nonEmpty) Attempt.failure(Err(s"A was decoded, but there were data remaining: ${remA.bytes}"))
            else Attempt.successful(DecodeResult(a, rem))
          }
        }
      }

      def encode(value: A) =
        codec.encode(value) flatMap { aBits => terminator.encode((aBits, ())) }

    }
  }




  /**
    * like vector[A], but instead consuming all bytes on decoding will try to consume all bytes that match for `A`
    * and will leave last bytes that did not match as remainder. May encode to Vector.empty if `A` did not match at all.
    * @param codec  Codec for `A`
    */
  def vectorV[A](codec: Codec[A]): Codec[Vector[A]] = {
    new Codec[Vector[A]] {
      def sizeBound: SizeBound = SizeBound.unknown

      def encode(value: Vector[A]): Attempt[BitVector] = {
        @tailrec
        def go(rem: Vector[A], acc: BitVector): Attempt[BitVector] = {
          rem.headOption match {
            case Some(a) =>
              codec.encode(a) match {
                case Attempt.Successful(bits) => go(rem.tail, acc ++ bits)
                case Attempt.Failure(err) => Attempt.failure(err)
              }

            case None => Attempt.successful(acc)
          }
        }
        go(value, BitVector.empty)
      }

      def decode(bits: BitVector): Attempt[DecodeResult[Vector[A]]] = {
        @tailrec
        def go(rem: BitVector, acc: Vector[A]): Attempt[DecodeResult[Vector[A]]] = {
          if (rem.isEmpty) Attempt.successful(DecodeResult(acc, BitVector.empty))
          else {
            codec.decode(rem) match {
              case Attempt.Successful(DecodeResult(a, rem)) => go(rem, acc :+ a)
              case Attempt.Failure(err) => Attempt.successful(DecodeResult(acc, rem))
            }
          }
        }
        go(bits, Vector.empty)
      }
    }
  }



  /**
    * Like VectorV, but the items are delimited by `Delimiter`.
    * Last item will not be terminated by `delimiter`
    * @param codec
    * @param delimiter
    * @param firstWithDelimiter if set to true, first element will start with delimiter
    * @tparam A
    * @return
    */
  def vectorVDelimited[A](codec: Codec[A], delimiter: Codec[Unit], firstWithDelimiter: Boolean = false): Codec[Vector[A]] = {
    new Codec[Vector[A]] {
      def sizeBound: SizeBound = SizeBound.unknown

      val withDelimiter = delimiter ~> codec

      def encode(value: Vector[A]): Attempt[BitVector] = {
        @tailrec
        def go(rem: Vector[A], acc: BitVector): Attempt[BitVector] = {
          rem.headOption match {
            case Some(a) =>
              val aCodec = if (rem.size == value.size && !firstWithDelimiter) codec else withDelimiter
              aCodec.encode(a) match {
                case Attempt.Successful(bits) => go(rem.tail, acc ++ bits)
                case Attempt.Failure(err) => Attempt.failure(err)
              }

            case None => Attempt.successful(acc)
          }
        }
        go(value, BitVector.empty)
      }

      def decode(bits: BitVector): Attempt[DecodeResult[Vector[A]]] = {
        @tailrec
        def go(rem: BitVector, acc: Vector[A]): Attempt[DecodeResult[Vector[A]]] = {
          if (rem.isEmpty) Attempt.successful(DecodeResult(acc, BitVector.empty))
          else {
            val aCodec = if (acc.isEmpty && !firstWithDelimiter) codec else withDelimiter
            aCodec.decode(rem) match {
              case Attempt.Successful(DecodeResult(a, rem)) => go(rem, acc :+ a)
              case Attempt.Failure(err) => Attempt.successful(DecodeResult(acc, rem))
            }
          }
        }
        go(bits, Vector.empty)
      }
    }
  }


  /** will encode a collection of `A` with min size of at least `sz`  **/
  def minItems[A, F[_] <: GenTraversable[_]](sz:Int)(codec: Codec[F[A]]): Codec[F[A]] = {
    guard(codec){ fa =>
      if (fa.size >= sz) None
      else Some(Err(s"Expected at least $sz items, got ${fa.size}"))
    }
  }

  /** will encode a collection of `A` with at max size of `sz` **/
  def maxItems[A, F[_] <: GenTraversable[_]](sz:Int)(codec: Codec[F[A]]): Codec[F[A]] = {
    guard(codec){ fa =>
      if (fa.size <= sz) None
      else Some(Err(s"Expected at max $sz items, got ${fa.size}"))
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

  /**
    * A maybe value, that should be encoded only if there is some value present.
    *
    * Tries to decode the value using the given codec, if the decode fails we return None.
    *
    * @param codec  The codec to attempt to be decoded/encoded
    */
  def maybe[A](codec: Codec[A]): Codec[Option[A]] = {
    fallback(
      provide(Option.empty[A])
      , codec.xmap[Some[A]](a => Some(a), _.get)
    ).xmap[Option[A]](_.merge, _.fold[Either[Option[A], Some[A]]](Left(Option.empty))(bv => Right(Some(bv))))
  }

  /**
    * Codec with a default value that is implied if there is no value in the supplied bits.
    *
    * If the value that is to be encoded is equal to the default value of the codec, then
    * we do not encode anything.
    *
    * @param codec  The codec to attempt to be decoded
    */
  def default[A](codec: Codec[A], default: A): Codec[A] = {
    maybe(codec).xmap(
      _.getOrElse(default)
      , a =>
        if (a == default) None
        else Some(a)
    )
  }

  /**
    * Codec for a set of a given `A`.
    *
    * @param codec The codec of `A` that is to be used for decoding values.
    */
  def set[A](codec: Codec[A]): Codec[Set[A]] = {
    scodec.codecs.list(codec).xmap(_.toSet, _.toList)
  }

}
