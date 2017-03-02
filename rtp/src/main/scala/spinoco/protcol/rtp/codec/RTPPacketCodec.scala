package spinoco.protcol.rtp.codec

import scodec.Codec
import scodec.codecs._
import shapeless.{::, HNil}
import spinoco.protcol.rtp.{RTPHeaderExtension, RTPPacket}

/**
  * Codec for RTP Packet
  */
object RTPPacketCodec {


  /*
   *
   *  0                   1                   2                   3
   *  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   * |V=2|P|X|  CC   |M|     PT      |       sequence number         |
   * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   * |                           timestamp                           |
   * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   * |           synchronization source (SSRC) identifier            |
   * +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
   * |            contributing source (CSRC) identifiers             |
   * |                             ....                              |
   * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *
   */


  lazy val codec : Codec[RTPPacket] =  {


    (impl.headerCodec
    .flatAppend { h =>
      vectorOfN(provide(h(3) /* CC */), int(32))
    }.flatAppend { h =>
      val x = h(2) // X
      if (x) impl.headerExtension.xmap[Some[RTPHeaderExtension]](Some(_), _.x).upcast[Option[RTPHeaderExtension]]
      else provide(None).upcast[Option[RTPHeaderExtension]]
    } :+ bytes)
    .xmap (
      { case v :: padding :: ext :: csrcCount :: marker :: pt :: seqNo :: ts :: ssrc :: csrc :: extensionHeader :: payload :: HNil =>
        val unpaddedPayload =
          if (! padding) payload
          else {
            val toDrop = payload.takeRight(1).toInt(signed = false)
            payload.dropRight(toDrop)
          }

        RTPPacket(
          version = v
          , marker = marker
          , payloadType = pt
          , sequenceNumber = seqNo
          , timestamp = ts
          , ssrc = ssrc
          , csrc = csrc
          , payload = unpaddedPayload
          , extensionHeader = extensionHeader
        )
      }
      , pkt => {
        val padded = (pkt.payload.size % 4).toInt
        val paddedPayload = pkt.payload ++ paddingMapBytes(padded) // padded may be only 0, 1, 2 or 3

        pkt.version :: (padded != 0) :: pkt.extensionHeader.nonEmpty :: pkt.csrc.size :: pkt.marker ::
          pkt.payloadType :: pkt.sequenceNumber :: pkt.timestamp :: pkt.ssrc :: pkt.csrc.toVector ::
          pkt.extensionHeader :: paddedPayload :: HNil
    })
  }

  object impl {

    lazy val headerCodec = {
      ("Version"          | version) ::
      ("Padding"          | bool) ::
      ("Extension"        | bool) ::
      ("CSRCCount"        | uint(4)) ::
      ("Marker"           | bool) ::
      ("Payload Type"     | uint(7)) ::
      ("Sequence number"  | uint(16)) ::
      ("Timestamp"        | ulong(32)) ::
      ("SSRC"             | int(32))
  }




    /*
     *  0                   1                   2                   3
     *  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * |      defined by profile       |           length              |
     * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * |                        header extension                       |
     * |                             ....                              |
     *
     */

    val headerExtension: Codec[RTPHeaderExtension] = {
      (
        ("Flag" | uint16) ::
          ("Extension content" | variableSizeBytes(wordSizeCodec, bytes))
        ).xmap (
        { case flag :: content :: HNil => RTPHeaderExtension(flag, content) }
        , { h => h.flag :: h.content :: HNil }
      )
    }
  }

}
