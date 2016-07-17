package spinoco.protocol.stun.codec

import scodec.bits._
import scodec.{Attempt, Codec}
import scodec.codecs._
import scodec.codecs.literals._
import shapeless.{::, HNil}
import spinoco.protocol.stun.TransactionId.BigIntTransaction
import spinoco.protocol.stun._


object StunMessageCodec {

  /*
   *  STUN MESSAGE:
   *
   *   0                   1                   2                   3
   *   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *  |0 0|     STUN Message Type     |         Message Length        |
   *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *  |                         Magic Cookie                          |
   *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *  |                                                               |
   *  |                     Transaction ID (96 bits)                  |
   *  |                                                               |
   *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *
   */

  /*
   *  STUN Message Type
   *
   *   0                   1
   *   2  3  4 5 6 7 8 9 0 1 2 3 4 5
   *  +--+--+-+-+-+-+-+-+-+-+-+-+-+-+
   *  |M |M |M|M|M|C|M|M|M|C|M|M|M|M|
   *  |11|10|9|8|7|1|6|5|4|0|3|2|1|0|
   *  +--+--+-+-+-+-+-+-+-+-+-+-+-+-+
   */

  def codec:Codec[StunMessage] = impl.message

  final val MAGIC_COOKIE = hex"0x2112A442".bits

  object impl {


    val DoubleZero = hex"00".bits

    val messageClass:Codec[MessageClass] = {
      mappedEnum(bits(2)
        , MessageClass.Request -> bin"00"
        , MessageClass.Indication -> bin"01"
        , MessageClass.Response(success = true) -> bin"10"
        , MessageClass.Response(success = false) -> bin"11"
      )
    }
    val stunMethod: Codec[StunMethod] = {
      mappedEnum(bits(12)
        , StunMethod.Binding -> bin"0b000000000001"
      )
    }
    val transactionId: Codec[TransactionId] = {
      bits(96).xmap(
        v => BigIntTransaction(BigInt(v.toByteArray))
        , _.toBytes.toBitVector
      )
    }
    val attributes:Codec[Vector[StunAttribute]] =
      vector(StunAttributeCodec.codec)

    val messageType:Codec[MessageClass :: StunMethod :: HNil] = {
      def decode(v:BitVector): Attempt[MessageClass :: StunMethod :: HNil] = {
        val methodBits = v.slice(0, 5) ++ v.slice(6, 9) ++ v.slice(10, 14)
        val clzBits = v.slice(5, 6) ++ v.slice(9, 10)


        stunMethod.decode(methodBits).flatMap { m =>
        messageClass.decode(clzBits).map { c =>
          c.value :: m.value :: HNil
        }}
      }

      def encode(in:MessageClass :: StunMethod :: HNil):Attempt[BitVector] = {
        messageClass.encode(in.head).flatMap { c =>
        stunMethod.encode(in.tail.head).map { m =>
          m.slice(0, 5) ++ c.slice(0, 1) ++ m.slice(5, 8) ++ c.slice(1, 2) ++ m.slice(8, 12)
        }}
      }

      bits(14).exmap[MessageClass :: StunMethod :: HNil](decode,encode)
    }

    val message = "STUN Message" | (
      ( "00"                  | DoubleZero) ::
      ( "Message Type"        | messageType) :::
      variableSizeBytes(uint16,
        ("Magic Cookie"       | MAGIC_COOKIE) ::
        ("Transaction Id "    | transactionId) ::
        ("Attributes"         | attributes)
      )
    ).as[StunMessage].complete



  }

}
