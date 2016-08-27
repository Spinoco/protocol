package spinoco.protocol.kafka.codec

import spinoco.protocol.common.util._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import org.xerial.snappy.{SnappyInputStream, SnappyOutputStream}
import scodec.Attempt
import scodec.bits.ByteVector

import scala.annotation.tailrec


object GZipCompression {

  /** deflates uncompressed bytes **/
  def deflate(bv:ByteVector):Attempt[ByteVector] =
    StreamCompression.inflate(bv)(new GZIPOutputStream(_))

  /** inflates compressed bytes **/
  def inflate(bv:ByteVector):Attempt[ByteVector] =
    StreamCompression.deflate(bv)(new GZIPInputStream(_))

}


object SnappyCompression {

  /** deflates uncompressed bytes **/
  def deflate(bv:ByteVector):Attempt[ByteVector] =
  StreamCompression.inflate(bv)(new SnappyOutputStream(_))

  /** inflates compressed bytes **/
  def inflate(bv:ByteVector):Attempt[ByteVector] =
  StreamCompression.deflate(bv)(new SnappyInputStream(_))

}


object StreamCompression {

  def inflate(bv:ByteVector)(mkOs:ByteArrayOutputStream => OutputStream):Attempt[ByteVector] = attempt {
    val bos = new ByteArrayOutputStream((bv.size/3).toInt*2) // ~ 30% compression ratio
    val os = mkOs(bos)
    try {
      os.write(bv.toArray)
    } finally {
      os.close()
    }
    ByteVector.view(bos.toByteArray)
  }


  def deflate(bv:ByteVector)(mkIs: ByteArrayInputStream => InputStream):Attempt[ByteVector] = attempt {
    val is = mkIs(new ByteArrayInputStream(bv.toArray))
    try {
      val buffSz: Int = (bv.size * 1.5).toInt
      @tailrec
      def go(curr: ByteVector): ByteVector = {
        // as buffer we use size of bv + 50% assuming compression ratio is always at least 30%
        val buff = Array.ofDim[Byte](buffSz)
        val read = is.read(buff)
        if (read == -1) curr
        else go(curr ++ ByteVector.view(buff).take(read))
      }
      go(ByteVector.empty)
    } finally {
      is.close()
    }
  }


}

