package spinoco.protocol.mgcp

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import shapeless.tag.@@
import spinoco.protocol.common.codec._
import spinoco.protocol.mgcp.MGCPParameter.ConnectionId

import scala.annotation.tailrec


package object codec {

  val `\r\n`: Codec[Unit] = constantString("\r\n")
  val `\n`: Codec[Unit] = constantString("\n")
  val WS: Codec[Unit] = dropWhile(BitVector(Array[Byte](' ')))(_.toChar.isWhitespace)
  val dropWS: Codec[Unit] = dropWhile(BitVector(Array[Byte]()))(_.toChar.isWhitespace)


  val transactionId: Codec[Int @@ MGCPTxId] =
    tagged(guard(intAsString) { i =>
      if (i < 0 || i > 999999999) Some(Err("Transaction id must be within [0, 999 999 999] range"))
      else None
    })


  val localEndpointNameCodec: Codec[LocalEndpointName] = {
    val partCodec: Codec[LocalEndpointPart] = {
      import LocalEndpointPart._
      ascii.xmap(
        {
          case "$" => `$`
          case "*" => `*`
          case other => NameString(other)
        }
        , {
          case `$` => "$"
          case `*` => "*"
          case NameString(s) => s
        }
      )
    }

    listDelimited(BitVector.view("/".getBytes), partCodec)
      .exmap(
        { ls =>
          ls.headOption match {
            case None => Attempt.failure(Err("At least one part of name is required"))
            case Some(h) => Attempt.successful(LocalEndpointName(h, ls.tail))
          }
        }
        , n => Attempt.successful(n.start +: n.parts)
      )
  }

  def isHex(ch:Char):Boolean =
    ch.isDigit || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')

  def is32Hex(field:String)(s:String):Option[Err] = {
    if (s.length > 32 || s.isEmpty) Some(Err(s"$field must have 1 - 32 hex characters, size is ${s.size}"))
    else if (s.exists(ch => ! isHex(ch))) Some(Err(s"$field must have 1 - 32 hex characters, but is '$s'}"))
    else None
  }

  val connectionIdCodec: Codec[String @@ ConnectionId] =
    tagged[String, ConnectionId](guard(ascii)(is32Hex("ConnectionId")))

  val domainCodec: Codec[String] = utf8



  val packageNameCodec: Codec[String] =
    guard(ascii)(s => if (s.nonEmpty) None else Some(Err(s"Package name must have at least one character: $s")))

  val eventSpecificationCodec: Codec[EventSpecification] = "Event Specification" | {

    val eventOwnerCodec: Codec[EventOwner] = {
      choice(
        constantString("$").decodeAs(EventOwner.`$`).upcast
        , constantString("*").decodeAs(EventOwner.`*`).upcast
        , connectionIdCodec.as[EventOwner.Connection].upcast
      )
    }

    (
      ("Package And Event" | takeWhileChar(PackageEventCodec.codec)('@')) ::
       ("Event Owner" | optional(recover2(constantString("@")), eventOwnerCodec) )
    ).as[EventSpecification]
  }

  /** encodes `A` until end of header. Returns bytes after the last nonEmpty line of header, including the empty line that ends the header **/
  def header[A](codec: Codec[A]): Codec[A] = {
    val cr = '\r'.toByte
    val lf = '\n'.toByte
    val crlfDouble = ByteVector.view("\r\n\r\n".getBytes)
    val lfDouble = ByteVector.view("\n\n".getBytes)
    new Codec[A] {
      def sizeBound: SizeBound = SizeBound.unknown
      def encode(value: A): Attempt[BitVector] = codec.encode(value)

      def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
        @tailrec
        def go(bytes: ByteVector): Attempt[DecodeResult[A]] = {
          def gethead(sepSize: Int) = {
            val orig = bits.bytes
            val toTake = orig.size - bytes.size
            orig.take(toTake + sepSize)
          }
          if (bytes.isEmpty) codec.decode(bits)
          else if (bytes.head == cr && bytes.startsWith(crlfDouble)) {
            codec.decode(gethead(2).bits).map { _.mapRemainder { _ => bytes.drop(2).bits } }
          } else if (bytes.head == lf && bytes.startsWith(lfDouble)) {
            codec.decode(gethead(1).bits).map { _.mapRemainder { _ => bytes.drop(1).bits } }
          } else go(bytes.tail)
        }
        go(bits.bytes)
      }
    }
  }

}
