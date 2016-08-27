package spinoco.protocol.kafka.codec

import java.util.zip.CRC32

import kafka.utils.CoreUtils
import spinoco.protocol.common.ProtocolSpec

/**
  * Tests for crc32 computation
  */
class Crc32Spec extends ProtocolSpec {

  def sunCrc32(b:Array[Byte]):Long = {
    val crc = new CRC32()
    crc.update(b)
    crc.getValue
  }


  "Crc32" - {

    "is computed correctly as in kafka utils"  in forAll { array: Array[Byte] =>
      val kafka = (CoreUtils.crc32(array) & 0xffffffffL).toInt
      val crc = MessageSetCodec.impl.computeCrc(array)
      kafka shouldBe crc
    }
  }

}
