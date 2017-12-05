package spinoco.protocol.rtp

import scodec.bits.ByteVector
import scodec.{Attempt, Codec, Err}
import scodec.codecs._

/**
  * Created by pach on 27/02/17.
  */
package object codec {

  val version: Codec[RTPVersion.Value] = enumerated(uint(2), RTPVersion)

  val wordSizeCodec: Codec[Int] = uint16.exmap(
    szWords => Attempt.successful(szWords*4)
    , sz =>
      if (sz % 4 == 0) Attempt.successful(sz/4)
      else Attempt.failure(Err(s"Expected size in words (%4 == 0) but got $sz"))
  )

  // helper to properly do padding. Padding index can be 0 .. 3
  val paddingMapBytes = Map(
    4 -> ByteVector.empty
    , 3 -> ByteVector.view(Array[Byte](1))
    , 2 -> ByteVector.view(Array[Byte](0, 2))
    , 1 -> ByteVector.view(Array[Byte](0, 0, 3))
    , 0 -> ByteVector.empty
  )

  val paddingMapBits = paddingMapBytes.mapValues(_.bits)

}
