package spinoco.protocol.kafka.codec

import java.util.zip.CRC32


import org.apache.kafka.common.utils.Crc32
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
      val kafka = (Crc32.crc32(array) & 0xffffffffL).toInt
      val crc = MessageSetCodec.impl.computeCrc(array)
      kafka shouldBe crc
    }
  }

}
