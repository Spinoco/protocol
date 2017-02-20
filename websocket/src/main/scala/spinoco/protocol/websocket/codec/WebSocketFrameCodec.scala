package spinoco.protocol.websocket.codec

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless.{::, HNil}
import spinoco.protocol.websocket.{OpCode, WebSocketFrame}



object WebSocketFrameCodec {

  val codec: Codec[WebSocketFrame] = impl.codec


  object impl {

    /*
     *   0                   1                   2                   3
     *   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     *  +-+-+-+-+-------+-+-------------+-------------------------------+
     *  |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
     *  |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
     *  |N|V|V|V|       |S|             |   (if payload len==126/127)   |
     *  | |1|2|3|       |K|             |                               |
     *  +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
     *  |     Extended payload length continued, if payload len == 127  |
     *  + - - - - - - - - - - - - - - - +-------------------------------+
     *  |                               |Masking-key, if MASK set to 1  |
     *  +-------------------------------+-------------------------------+
     *  | Masking-key (continued)       |          Payload Data         |
     *  +-------------------------------- - - - - - - - - - - - - - - - +
     *  :                     Payload Data continued ...                :
     *  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
     *  |                     Payload Data continued ...                |
     *  +---------------------------------------------------------------+
     *
     *
     */

    val opcodeCodec: Codec[OpCode.Value] = {
      uint(4).exmap(
        {
          case 0 => Attempt.successful(OpCode.Continuation)
          case 1 => Attempt.successful(OpCode.Text)
          case 2 => Attempt.successful(OpCode.Binary)
          case 8 => Attempt.successful(OpCode.Close)
          case 9 => Attempt.successful(OpCode.Ping)
          case 0xA => Attempt.successful(OpCode.Pong)
          case other => Attempt.Failure(Err(s"Invalid OpCode $other"))
        }
        , opcode => Attempt.successful(opcode.id)
      )
    }



    // this supports only size of Long.MaxValue.
    // In theory we may have size of unsigned long, which aren't supported by this implementation
    // anyhow the max value of the payload supported is Int.MaxValue, that is about 2G
    def payloadLength:Codec[Int] = {

      ("Length Header" | uint(7)).flatZip {
        case sz if sz <= 125 => constant(ByteVector.empty).xmap(_ => sz, (_: Int) => ())
        case sz if sz == 126 =>
          long(16).exmap[Int] (
            lsz => Attempt.successful(lsz.toInt)
            , i => Attempt.successful(i.toLong)
          )
        case sz =>
          long(64).exmap[Int] (
            lsz => if (lsz <= Int.MaxValue)  Attempt.successful(lsz.toInt) else Attempt.failure(Err(s"Max supported size is ${Int.MaxValue}, got $lsz"))
            , i => Attempt.successful(i.toLong)
          )

      }.exmap(
        { case (_, sz) => Attempt.successful(sz)  }
        , {
          case sz if sz <= 125 => Attempt.successful((sz, sz))
          case sz if sz > 125 && sz <= 65535 => Attempt.successful((126, sz))
          case sz if sz < Int.MaxValue  => Attempt.successful((127, sz))
          case _ => Attempt.failure(Err("Only payloads size of <= Int.MaxValue are supported"))
        }
      )


    }

    def unmaskedPayload(sz:Int):Codec[(ByteVector,Option[Int])] = {
      ("PAYLOAD-UNMASKED" | bytes(sz)).xmap (
        bs => bs -> None
        , { case (bs, _) => bs }
      )
    }

    def maskedPayload(sz:Int):Codec[(ByteVector,Option[Int])] = {

      def xor(mask: BitVector, payload:ByteVector):ByteVector = {
        val maskBytes = mask.bytes
        val result = Array.ofDim[Byte](payload.length.toInt)
        var i = 0
        payload.foreach { b =>
          val j = i % 4
          result(i) = (b ^ maskBytes(j)).toByte
          i += 1
        }
        ByteVector.view(result)
      }

      (("MASK"        | int(32)) ~
        ("PAYLOAD"    | bytes(sz))
      ).exmap(
        { case (mask,bs) => Attempt.successful((xor(BitVector.fromInt(mask), bs), Some(mask))) }
        ,
        {
          case (bs, None) =>  Attempt.failure(Err("Mask is required, but none specified"))
          case (bs, Some(mask)) =>  Attempt.successful((mask, xor(BitVector.fromInt(mask), bs)))
        }
      )
    }

    def rsv: Codec[(Boolean, Boolean, Boolean)] = {
      import shapeless.syntax.std.tuple._
      (("1" | bool) :: ("2" | bool) :: ("3" | bool)).xmap(_.tupled, _.productElements)
    }



    def codec: Codec[WebSocketFrame] = {

      (("FIN"         |  bool) ::
      ("RSV"          |  rsv) ::
      ("OPCODE"       |  opcodeCodec) ::
      ("MASK"         |  bool) ::
      ("PAYLOAD LEN"  |  payloadLength))
      .flatZip {
        case fin :: rsv :: opcode :: mask :: len :: HNil =>
          if (! mask)  unmaskedPayload(len)
          else maskedPayload(len)
      }.xmap ({ case (fin :: rsv :: opcode :: maskFlag :: len :: HNil, (payload, mask)) =>
        WebSocketFrame(
          fin = fin
          , rsv = rsv
          , opcode = opcode
          , payload = payload
          , mask = mask.filter(_ => maskFlag)
        )
      }, wsf =>
        (wsf.fin :: wsf.rsv :: wsf.opcode :: wsf.mask.nonEmpty :: wsf.payload.size.toInt :: HNil, (wsf.payload, wsf.mask))
      )
    }




  }


}
