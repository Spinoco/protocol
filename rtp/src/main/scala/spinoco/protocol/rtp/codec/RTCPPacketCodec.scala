package spinoco.protocol.rtp.codec

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import shapeless.{::, HNil}
import spinoco.protocol.rtp.RTCPPacketBody._
import spinoco.protocol.rtp._

import scala.annotation.tailrec


object RTCPPacketCodec {
  import impl._

  // todo: padding support of extension

  lazy val codec: Codec[RTCPPacket] = new Codec[RTCPPacket] {
    def sizeBound: SizeBound = SizeBound.unknown

    def decode(bits: BitVector): Attempt[DecodeResult[RTCPPacket]] = {
      rtcpHeaderCodec.decode(bits).flatMap { r =>
        val header = r.value
        val (packet, rem) = bits.drop(32).splitAt(header.length*8) // remove header bytes and take packet body
        // packet body may be padded, in that case strip padding here
        val unpadded =
        if (! header.padding) packet
        else {
          val toDrop = packet.takeRight(8).toInt(signed = false)
          packet.dropRight(toDrop*8)
        }

        (r.value.tpe match {
          case RTCPPacketType.SenderReport =>  senderReportCodec(header.elements).decodeValue(unpadded)
          case RTCPPacketType.ReceiverReport => receiverReportCodec(header.elements).decodeValue(unpadded)
          case RTCPPacketType.SourceDescription => sourceDescriptionCodec(header.elements).decodeValue(unpadded)
          case RTCPPacketType.Bye => byeCodec(header.elements).decodeValue(unpadded)
          case RTCPPacketType.AppData => appDataCodec.decodeValue(unpadded)
        }).map { body =>
          DecodeResult(
            RTCPPacket(header.version, body)
            , rem
          )
        }
      }
    }

    def encode(value: RTCPPacket): Attempt[BitVector] = {
      val (bodyAttempt, elements, tpe ) =
      value.body match {
        case sr: RTCPPacketBody.RTCPSenderReport =>
          (senderReportCodec(sr.report.size).encode(sr), sr.report.size,  RTCPPacketType.SenderReport)

        case rr: RTCPPacketBody.RTCPReceiverReport =>
          (receiverReportCodec(rr.report.size).encode(rr), rr.report.size,  RTCPPacketType.ReceiverReport)

        case sd: RTCPPacketBody.SourceDescription =>
          (sourceDescriptionCodec(sd.descriptors.size).encode(sd), sd.descriptors.size, RTCPPacketType.SourceDescription)

        case bye: RTCPPacketBody.Bye =>
          (byeCodec(bye.ids.size).encode(bye), bye.ids.size, RTCPPacketType.Bye)

        case app: RTCPPacketBody.ApplicationData =>
          (appDataCodec.encode(app), 0, RTCPPacketType.AppData)
      }

      bodyAttempt.flatMap { body =>
        val padding = (body.size / 8).toInt % 4
        val paddedBody = body ++ paddingMapBits(padding)
        val szBytes = (paddedBody.size / 8).toInt
        val header = RTCPHeader(value.version, padding != 0, elements, szBytes + 4, tpe)
        rtcpHeaderCodec.encode(header)
        .map { _ ++ paddedBody }
      }
    }


  }



  object impl {

    /*
     *         0                   1                   2                   3
     *         0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * header |V=2|P|    RC   |   PT=SR=200   |             length            |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                         SSRC of sender                        |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * sender |              NTP timestamp, most significant word             |
     * info   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |             NTP timestamp, least significant word             |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                         RTP timestamp                         |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                     sender's packet count                     |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                      sender's octet count                     |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * report |                 SSRC_1 (SSRC of first source)                 |
     * block  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *   1    | fraction lost |       cumulative number of packets lost       |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |           extended highest sequence number received           |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                      interarrival jitter                      |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                         last SR (LSR)                         |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                   delay since last SR (DLSR)                  |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * report |                 SSRC_2 (SSRC of second source)                |
     * block  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *   2    :                               ...                             :
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     *        |                  profile-specific extensions                  |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *
     */
    def senderReportCodec(sections: Int): Codec[RTCPSenderReport] = "Sender Report" | {
      ( ("SSRC"                 | int32) ::
        ("NTP timestamp MSW"    | long(32)) ::
        ("NTP timestamp LSW"    | long(32)) ::
        ("RTP timestamp"        | ulong(32)) ::
        ("Packet Count"         | ulong(32)) ::
        ("Octet Count"          | ulong(32)) ::
        ("Reports"              | vectorOfN(provide(sections), reportBlockCodec)) ::
        ("Extension"            | bytes)
        ).as[RTCPSenderReport]
    }


    /*
     *
     *         0                   1                   2                   3
     *         0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * header |V=2|P|    RC   |   PT=RR=201   |             length            |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                     SSRC of packet sender                     |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * report |                 SSRC_1 (SSRC of first source)                 |
     * block  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *   1    | fraction lost |       cumulative number of packets lost       |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |           extended highest sequence number received           |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                      interarrival jitter                      |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                         last SR (LSR)                         |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                   delay since last SR (DLSR)                  |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * report |                 SSRC_2 (SSRC of second source)                |
     * block  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *   2    :                               ...                             :
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     *        |                  profile-specific extensions                  |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *
     *
     */

    def receiverReportCodec(sections: Int): Codec[RTCPReceiverReport] = "Receiver Report" | {
      ( ("SSRC"                 | int32) ::
        ("Reports"              | vectorOfN(provide(sections), reportBlockCodec)) ::
        ("Extension"            | bytes)
        ).as[RTCPReceiverReport]

    }


    /*
     *         0                   1                   2                   3
     *         0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * header |V=2|P|    SC   |  PT=SDES=202  |             length            |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * chunk  |                          SSRC/CSRC_1                          |
     *   1    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                           SDES items                          |
     *        |                              ...                              |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * chunk  |                          SSRC/CSRC_2                          |
     *   2    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                           SDES items                          |
     *        |                              ...                              |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     */

    def sourceDescriptionCodec(elements: Int): Codec[SourceDescription] = {
      ("Source Description" | vectorOfN(provide(elements), sourceDescriptorCodec)).as[SourceDescription]
    }

    /*
     *        0                   1                   2                   3
     *        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     *       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *       |V=2|P|    SC   |   PT=BYE=203  |             length            |
     *       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *       |                           SSRC/CSRC                           |
     *       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *       :                              ...                              :
     *       +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * (opt) |     length    |               reason for leaving            ...
     *       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *
     *
     */

    def byeCodec(elements: Int): Codec[Bye] = "Bye" | {
      val reasonCodec: Codec[Option[String]] = new Codec[Option[String]] {
        val stringCodec = variableSizeBits(uint8, ascii)
        def decode(bits: BitVector): Attempt[DecodeResult[Option[String]]] = {
          if (bits.isEmpty) Attempt.successful(DecodeResult(None, bits))
          else stringCodec.decode(bits).map(_.map(Some(_)))
        }

        def encode(value: Option[String]): Attempt[BitVector] = {
          value match {
            case None => Attempt.successful(BitVector.empty)
            case Some(s) => stringCodec.encode(s)
          }
        }

        def sizeBound: SizeBound = SizeBound.unknown
      }

      (
        ("SSRC or CSRC"   | vectorOfN(provide(elements), int32)) ::
        ("Reason"         | reasonCodec)
      ).as[Bye]
    }

    /*
     *
     *     0                   1                   2                   3
     *     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     *    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *    |V=2|P| subtype |   PT=APP=204  |             length            |
     *    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *    |                           SSRC/CSRC                           |
     *    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *    |                          name (ASCII)                         |
     *    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *    |                   application-dependent data                ...
     *    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     */
    def appDataCodec: Codec[ApplicationData] = "AppData" | {
      ( ("SSRC or CSRC" | int32 ) ::
        ("Name"         | fixedSizeBytes(4, ascii)) ::
        ("Data"         | bytes)
      ).as[ApplicationData]
    }

    lazy val rtcpHeaderCodec: Codec[RTCPHeader] = "Header" | {


      ( ("Version"                | version) ::
        ("Padding"                | bool) ::
        ("Block Count"            | uint(5)) ::
        ("Packet Type"            | enumerated(uint8, RTCPPacketType) ) ::
        ("Length"                 | wordSizeCodec.xmap[Int](_ + 4, _ - 4))
      ).xmap(
        { case version :: padding :: elements :: tpe :: length :: HNil =>
          RTCPHeader(
            version = version
            , padding = padding
            , elements = elements
            , length = length
            , tpe = tpe
          )
        }
        , { h => h.version :: h.padding :: h.elements :: h.tpe :: h.length :: HNil }
      )
    }

    /*
     *         0                   1                   2                   3
     *         0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     * report |                 SSRC_1 (SSRC of first source)                 |
     * block  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *   1    | fraction lost |       cumulative number of packets lost       |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |           extended highest sequence number received           |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                      interarrival jitter                      |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                         last SR (LSR)                         |
     *        +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     *        |                   delay since last SR (DLSR)                  |
     *        +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
     *
     */
    lazy val reportBlockCodec: Codec[Report]  = {
      ( ("SSRC or CSRC"         | int32) ::
        ("Fraction"             | uint8)  ::
        ("Packet lost"          | uint24) ::
        ("Hi Seq No Received"   | ulong(32)) ::
        ("Jitter"               | int(32) ) ::
        ("LSR"                  | ulong(32)) ::
        ("DLSR"                 | ulong(32))
      ).as[Report]
    }


    lazy val sourceDescriptorCodec: Codec[SourceDescriptor] = {
      val attributes: Codec[Vector[(SourceDescriptorType.Value, String)]] =
        new Codec[Vector[(SourceDescriptorType.Value, String)]] {
          val tpeCodec:Codec[SourceDescriptorType.Value] =
            enumerated(uint8, SourceDescriptorType)

          val tupleCodec: Codec[(SourceDescriptorType.Value, String)] =
            ("Type"   | tpeCodec) ~
            ("Value"  | variableSizeBytes(uint8, ascii))

          val allAttributes = vector(tupleCodec)

          val paddings = Map(
           0 -> ByteVector(0, 0, 0, 0).bits // rounded to 4, add END a 3 pads
           , 3 -> ByteVector(0).bits  // just add END
           , 2 -> ByteVector(0, 0).bits // END + one pad.
           , 1 -> ByteVector(0, 0, 0).bits  // end + 2 pad.
          )




          def sizeBound: SizeBound = SizeBound.unknown

          def decode(bits: BitVector): Attempt[DecodeResult[Vector[(SourceDescriptorType.Value, String)]]] = {
            @tailrec
            def go(remains: BitVector, acc: Vector[(SourceDescriptorType.Value, String)]): Attempt[DecodeResult[Vector[(SourceDescriptorType.Value, String)]]] = {
              if (remains.isEmpty) Attempt.failure(Err("Expected type of attribute, but none found"))
              else {
                val tpe = remains.take(8).toInt(signed = false)
                if (tpe == 0) {
                  // end of list indicated
                  // we have to strip any padding to 32 bits and return remainder
                  val toDrop = ((bits.size - remains.size) * 8) % 4
                  if (remains.size  < toDrop)
                    Attempt.failure(Err(s"Remaining bytes are not aligned to 32 bit boundary, wanted to drop $toDrop, but only ${remains.size} present"))
                  else
                    Attempt.successful(DecodeResult(acc, remains.drop(toDrop)))

                } else {
                    tupleCodec.decode(remains) match {
                      case Attempt.Failure(err) => Attempt.Failure(err)
                      case Attempt.Successful(DecodeResult(descriptor, remainder)) =>
                        go(remainder, acc :+ descriptor)
                    }
                }

              }
            }

            go(bits, Vector.empty)
          }

          def encode(value: Vector[(SourceDescriptorType.Value, String)]): Attempt[BitVector] = {
            allAttributes.encode(value).map { encoded =>
              val sz = encoded.size / 8
              val pad = sz % 4
              val padBits = paddings(pad.toInt) // pad is guaranteed to be 0,1,2,3 only
              encoded ++ padBits
            }
          }


        }

      ( ("SSRC"         | int32       ) ::
        ("Attributes "  | attributes  )
      ).as[SourceDescriptor]
    }

  }

}
