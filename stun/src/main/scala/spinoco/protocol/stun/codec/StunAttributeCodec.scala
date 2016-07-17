package spinoco.protocol.stun.codec


import java.net.{Inet4Address, Inet6Address, InetAddress, InetSocketAddress}

import scodec.codecs._
import scodec.{Attempt, Codec}
import scodec.bits.ByteVector
import spinoco.protocol.stun.{ErrorCause, StunAttribute}
import spinoco.protocol.common._
import spinoco.protocol.common.codec._



object StunAttributeCodec {

  def codec:Codec[StunAttribute] = impl.supportedAttribute


  object impl  {
    import StunAttribute._

    /*
       STUN Attribute
       0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |         Type                  |            Length             |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |                         Value (variable)                ....
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

     */




    /*
       Mapped Address

       0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |0 0 0 0 0 0 0 0|    Family     |           Port                |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |                                                               |
      |                 Address (32 bits or 128 bits)                 |
      |                                                               |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


     */


    val ipv4Address: Codec[Inet4Address] = {
      "IPV4Address" | bytes(4).exmap[Inet4Address](
        bits => util.attempt(InetAddress.getByAddress(bits.toArray).asInstanceOf[Inet4Address])
        , f => Attempt.successful(ByteVector(f.getAddress))
      )
    }

    val ipv6Address: Codec[Inet6Address] = {
      "IPV6Address" | bytes(32).exmap[Inet6Address](
        bits => util.attempt(InetAddress.getByAddress(bits.toArray).asInstanceOf[Inet6Address])
        , f => Attempt.successful(ByteVector(f.getAddress))
      )
    }

    val socketAddress:Codec[InetSocketAddress] = {
      "Ignored" | ignore(8) ~>
        ("SocketAddress" | discriminated[(Int,InetAddress)].by("Family" | uint8)
        .| (1) { case in@(port,addr:Inet4Address) => in } (identity)(("Port" | uint16) ~ ipv4Address.upcast[InetAddress])
        .| (2) { case in@(port,addr:Inet6Address) => in } (identity)(("Port" | uint16) ~ ipv6Address.upcast[InetAddress])
        ).xmap(
          {case (port, addr) => new InetSocketAddress(addr, port) }
          , sa => (sa.getPort,sa.getAddress)
        )
    }

    val mappedAddress:Codec[MappedAddress] =
      "Mapped Address" | attribute {
        socketAddress.xmap(MappedAddress.apply,_.address)
      }


    /*
      XOR Mapped address

      0                   1                   2                   3
      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |x x x x x x x x|    Family     |         X-Port                |
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |                X-Address (Variable)
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

     */


    val mappedAddressXor:Codec[XorMappedAddress] = {
      val xPort:Codec[Int] =
        "X-Port" | xor(uint16,StunMessageCodec.MAGIC_COOKIE)

      "Mapped Address XOR" | attribute {
        ("Ignored" | ignore(8)) ~>
          ("SocketAddress" | discriminated[XorMappedAddress].by("Family" | uint8)
            .|(1) { case XorMappedAddress(p, bs) if bs.size == 4 => (p, bs) }(XorMappedAddress.apply _ tupled)(xPort ~ bytes(4))
            .|(2) { case XorMappedAddress(p, bs) if bs.size == 32 => (p, bs) }(XorMappedAddress.apply _ tupled)(xPort ~ bytes(32))
            )
      }

    }


    val userName:Codec[UserName] = {
      "User Name" | variableSizeBytes("Length" | intBounded(uint16)(1,513)
        , utf8.xmap(UserName.apply,_.authorization)
      )
    }



    val messageIntegrity:Codec[MessageIntegrity] = {
      "Message Integrity" | attribute {
        bytes(20).xmap(MessageIntegrity.apply,_.hash)
      }
    }

    val fingerPrint:Codec[FingerPrint] = {
      "FingerPrint" | attribute {
        uint32.xmap(FingerPrint.apply,_.crc32)
      }
    }

    /*
      ErrorCode Attribute

       0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |           Reserved, should be 0         |Class|     Number    |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |      Reason Phrase (variable)                                ..
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

     */

    val errorCode:Codec[ErrorCode] = {
      val clzNumber:Codec[Int] = {
        (("Class"    | intBounded(int(3))(3,6) ) ~
        ( "Number"   | intBounded(int(8))(0,99)  ))
        .xmap(
          { case (clz, num) => clz*100 + num }
          , int => (int / 100, int % 100)
        )
      }

      "Error Code" | attribute {
        (("Ignored"  | ignore(21)) ~>
        ( "Code"     | enumerated(clzNumber,ErrorCause) ) ~
        ( "Phrase"   | stringBounded(utf8)(0,128) ))
        .xmap(ErrorCode.apply _ tupled, ec => (ec.cause, ec.phrase))
      }

    }

    val realm:Codec[Realm] = {
      "Realm" | attribute {
        stringBounded(utf8)(0,128).xmap(Realm.apply,_.realm)
      }
    }

    val nonce:Codec[Nonce] = {
      "Nonce" | attribute {
        stringBounded(utf8)(0,128).xmap(Nonce.apply,_.nonce)
      }
    }

    val unknownAttributes:Codec[UnknownAttributes] = {
      "Unknown Attribute" | attribute {
        vector(uint16).xmap(UnknownAttributes.apply, _.attributes)
      }
    }

    val software:Codec[Software] =  {
      "Software" | attribute {
        stringBounded(utf8)(0,128).xmap(Software.apply,_.description)
      }
    }


    val alternateServer:Codec[AlternateServer] =
      "Alternate Server" | attribute {
        socketAddress.xmap(AlternateServer.apply,_.address)
      }


    val priority:Codec[Priority] = {
      "Priority" | attribute {
        uint32.xmap(Priority.apply,_.priority)
      }
    }

    val useCandidate:Codec[UseCandidate.type] = {
      "Use Candidate" | attribute {
        bytes(0).xmap(_ => UseCandidate, _ => ByteVector.empty)
      }
    }


    val iceControlled:Codec[IceControlled] = {
      "Ice Controlled" | attribute {
        bits(64).xmap(v => IceControlled(BigInt(v.toByteArray)),ice => ByteVector.view(ice.rnd.toByteArray).toBitVector)
      }
    }

    val iceControlling:Codec[IceControlling] = {
      "Ice Controlling" | attribute {
        bits(64).xmap(v => IceControlling(BigInt(v.toByteArray)),ice => ByteVector.view(ice.rnd.toByteArray).toBitVector)
      }
    }



    def attribute[A <: StunAttribute](codec:Codec[A]):Codec[A] =
      variableSizeBytes("Length" | uint16, "Attribute" | codec)






    val supportedAttribute =
      "STUN Attribute" | discriminated[StunAttribute].by("Attribute type" | uint16)
      .| (0x0001) { case a:MappedAddress => a } (identity)(mappedAddress)
      .| (0x0006) { case a:UserName => a } (identity)(userName)
      .| (0x0008) { case a:MessageIntegrity => a } (identity)(messageIntegrity)
      .| (0x0009) { case a:ErrorCode => a } (identity)(errorCode)
      .| (0x000A) { case a:UnknownAttributes => a } (identity)(unknownAttributes)
      .| (0x0014) { case a:Realm => a } (identity)(realm)
      .| (0x0015) { case a:Nonce => a } (identity)(nonce)
      .| (0x0020) { case a:XorMappedAddress => a } (identity)(mappedAddressXor)
      .| (0x8023) { case a:AlternateServer => a } (identity)(alternateServer)
      .| (0x8022) { case a:Software => a } (identity)(software)
      .| (0x8028) { case a:FingerPrint => a } (identity)(fingerPrint)
      .| (0x0024) { case a:Priority => a } (identity)(priority)
      .| (0x0025) { case UseCandidate => UseCandidate } (identity)(useCandidate)
      .| (0x8029) { case a:IceControlled => a } (identity)(iceControlled)
      .| (0x802A) { case a:IceControlling => a } (identity)(iceControlling)
  }

}
