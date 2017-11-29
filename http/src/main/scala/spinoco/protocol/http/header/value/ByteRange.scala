package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.common.Terminator
import spinoco.protocol.http.codec.helper._

/**
  * Created by pach on 12/01/17.
  */
sealed trait ByteRange

object ByteRange {

  sealed case class Slice(first: Long, last: Long) extends ByteRange

  sealed case class FromOffset(offset: Long) extends ByteRange

  sealed case class Suffix(length: Long) extends ByteRange


  val codec : Codec[ByteRange] = {
    asciiConstant("bytes=") ~> bytesWsRemoved.codedAs(choice(
      (terminated(longAsString, Terminator.constantString1("-")) ~ longAsString).xmap[Slice](Slice.apply _ tupled, s => s.first -> s.last).upcast
      , (bytesUntil(_ != '-').codedAs(longAsString) <~ asciiConstant("-")).xmap[FromOffset](FromOffset.apply, _.offset).upcast
      , (asciiConstant("-") ~> longAsString).xmap[Suffix](Suffix.apply, _.length ).upcast
    ))
  }

}
