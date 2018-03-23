package spinoco.protocol.asn

import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.DiscriminatorCodec
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

import scala.util.Try


package object ber {

  /**
    * Codec for BER identifier octets.
    *
    * As of now we only support number tag in range of 0 - 30, as such we do not
    * support the extended identifier octets.
    */
  private[ber] val identifier: Codec[Identifier] = {
    new Codec[Identifier] {

      def decode(bits: BitVector): Attempt[DecodeResult[Identifier]] = {
        Attempt.fromEither(
          (for {
            classTagBits <- bits.acquire(2).right
            classTag <- Try(ClassTag(classTagBits.toInt(false))).toEither.left.map(_.toString).right
            constructed = bits.get(2)
            remaining = bits.drop(3)
            numberTagBits <- remaining.acquire(5).right
            numberTag = numberTagBits.toInt(false)
            _ <-
              if (numberTag >= 31) Left("Tag number can only be 0 - 30, we do not support the extended identifier octets").right
              else Right(()).right
          } yield DecodeResult(Identifier(classTag, constructed, numberTag), remaining.drop(5)))
            .left.map(Err(_))
        )
      }

      def encode(value: Identifier): Attempt[BitVector] = {
        if (value.numberTag <= 30 && value.numberTag >= 0) {
          Attempt.successful(
            BitVector.fromInt(value.classTag.id, 2) ++
              BitVector.bit(value.constructed) ++
              BitVector.fromInt(value.numberTag, 5)
          )
        } else {
          Attempt.failure(Err("Tag number can only be 0 - 30 (only ASN.1 native values) but was: " + value))
        }
      }

      //This is size of one octet, does not support more that 30 tagNumbers
      def sizeBound: SizeBound = SizeBound.exact(8)
    }
  }

  /**
    * Codec for BER length octets.
    *
    * This can specify length of 2 ^ 127 (1.7014118e+38) octets of the content octets.
    *
    * This as well supports infinite length octets, which are denoted by None.
    *
    */
  private[ber] val length: Codec[Option[Long]] = {
    new Codec[Option[Long]] {
      def decode(bits: BitVector): Attempt[DecodeResult[Option[Long]]] = {
        if (bits.size < 8) {
          Attempt.failure(Err.insufficientBits(8, bits.size))
        } else if (bits.get(0)) {
          val sizeOctets = bits.sliceToInt(1, 7, false)
          if (sizeOctets == 0) {
            Attempt.successful(DecodeResult(None, bits.drop(8)))
          } else if (sizeOctets == 127) {
            Attempt.failure(Err("The length is reserved value"))
          } else if (bits.size < sizeOctets * 8){
            Attempt.failure(Err.insufficientBits(sizeOctets * 8, bits.size))
          } else {
            Attempt.successful(DecodeResult(Some(bits.sliceToLong(8, sizeOctets * 8, false)), bits.drop(8 + sizeOctets * 8)))
          }
        } else {
          Attempt.successful(DecodeResult(Some(bits.sliceToLong(1, 7, false)), bits.drop(8)))
        }
      }

      def encode(value: Option[Long]): Attempt[BitVector] = {
        value match {
          case None =>
            Attempt.successful(BitVector.bit(true) ++ BitVector.fromInt(0, 7))
          case Some(value) =>
            if (value < 0) {
              Attempt.failure(Err("Length cannot be zero"))
            } else if (value <= 127) {
              Attempt.successful(BitVector.bit(false) ++ BitVector.fromInt(value.toInt, 7))
            } else {
              val octets = (value.toHexString.length / 2).ceil.toInt

              if (octets > 126) {
                Attempt.failure(Err("Cannot BER encode anything with size that has more than 126 octets"))
              } else {
                Attempt.successful(BitVector.bit(true) ++ BitVector.fromInt(octets, 7) ++ BitVector.fromLong(value, octets * 8))
              }
            }
        }
      }

      def sizeBound: SizeBound = SizeBound.unknown
    }
  }

  /**
    * Wraps a given codec with BER length octets.
    *
    * We guard the decode of the value by reading whether we have enough of data
    * before hand of the actual decode.
    *
    * Id there is not enough data supplied then the decode will end with [[Err.InsufficientBits]].
    *
    * @param codec  The codec to be used for the data specified by the length octets.
    */
  def finiteLength[A](codec: Codec[A]): Codec[A] = {
    new Codec[A] {
      def decode(bits: BitVector) = {
        for {
          lengthResult <- length.decode(bits)
          a <- lengthResult.value match {
            case None => Attempt.failure(Err("Can only decode finite size elements, received infinite"))
            case Some(length) =>
              val bitsToBeUsed = length * 8
              if (bitsToBeUsed > lengthResult.remainder.size) {
                Attempt.failure(Err.insufficientBits(bitsToBeUsed, lengthResult.remainder.size))
              } else {
                val (use, remainder) = lengthResult.remainder.splitAt(bitsToBeUsed)
                codec.decode(use).map(_.copy(remainder = remainder))
              }
          }
        } yield a
      }

      def encode(value: A) = {
        for {
          encodeA <- codec.encode(value)
          encodeLength <- length.encode(Some(encodeA.size / 8))
        } yield encodeLength ++ encodeA
      }

      def sizeBound = codec.sizeBound.atLeast
    }
  }

  /**
    * Codec for a single BER encoded codec.
    *
    * @param classTag     The class tag of the value that is to be encoded.
    * @param constructed  Whether the codec is constructed, ie is made out of other BER encoded data.
    * @param numberTag    The number tag to specify the data within the class tag scope.
    * @param codec        The codec for the data withing this BER tag.
    */
  def codecSingle[A](classTag: ClassTag.Value, constructed: Boolean, numberTag: Int)(codec: Codec[A]): Codec[A] = {
    val identifierValue = Identifier(classTag, constructed, numberTag)
    identifier.consume[A] {
      case `identifierValue` => finiteLength(codec)
      case other => scodec.codecs.fail(Err(s"Expected number tag: $numberTag but gotten $other"))
    }{_ => identifierValue}
  }

  /** A codec for some `A` that is discriminated by the BER identifier. */
  def discriminated[A]: DiscriminatorCodec[A, Identifier] = scodec.codecs.discriminated[A].by(identifier)


  // Universal codecs

  val endOfContent: BitVector = BitVector.fromInt(0xFF, 1)

  val boolean: Codec[Boolean] = codecSingle(ClassTag.Universal, false, 1)(scodec.codecs.bool)

  lazy val integer: Codec[Int] = codecSingle(ClassTag.Universal, false, 2)(scodec.codecs.vint)

  val bitStringPrimitive: Codec[BitVector] = codecSingle(ClassTag.Universal, false, 3)(scodec.codecs.bits)
  val bitStringConstructed: Codec[BitVector] = codecSingle(ClassTag.Universal, true, 3)(scodec.codecs.bits)

  val octetStringPrimitive: Codec[ByteVector] = codecSingle(ClassTag.Universal, false, 4)(scodec.codecs.bytes)
  val octetStringConstructed: Codec[ByteVector] = codecSingle(ClassTag.Universal, true, 4)(scodec.codecs.bytes)

  val NULL: Codec[Unit] = codecSingle(ClassTag.Universal, false, 5)(scodec.codecs.provide(()))

  val objectIdentifier: Codec[String] = codecSingle(ClassTag.Universal, false, 6)(scodec.codecs.utf8)

  val objectDescriptor: Codec[String] = codecSingle(ClassTag.Universal, false, 7)(scodec.codecs.utf8)

  val external: Codec[String] = codecSingle(ClassTag.Universal, true, 8)(scodec.codecs.utf8)

  val real: Codec[Float] = codecSingle(ClassTag.Universal, false, 9)(scodec.codecs.float)

  val enumerated: Codec[Int] = codecSingle(ClassTag.Universal, false, 10)(scodec.codecs.vint)

  val embeddedPDV: Codec[String] = codecSingle(ClassTag.Universal, true, 11)(scodec.codecs.utf8)

  val utf8StringPrimitive: Codec[String] = codecSingle(ClassTag.Universal, false, 12)(scodec.codecs.utf8)
  val utf8StringConstructed: Codec[String] = codecSingle(ClassTag.Universal, true, 12)(scodec.codecs.utf8)

  val relativeOID: Codec[String] = codecSingle(ClassTag.Universal, false, 13)(scodec.codecs.utf8)

  //14 - 15 reserved

  def sequence[A](consume: Codec[A]): Codec[A] = codecSingle(ClassTag.Universal, true, 16)(consume)

  def set[A](consume: Codec[A]): Codec[A] = codecSingle(ClassTag.Universal, true, 17)(consume)


}
