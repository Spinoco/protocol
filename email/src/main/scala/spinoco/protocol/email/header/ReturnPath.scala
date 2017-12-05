package spinoco.protocol.email.header

import scodec.{Attempt, Codec, Err}

/**
  * RFC 5322 3.6.7:
  *
  * The "Return-Path:" header field contains a pair of angle brackets
  *  that enclose an optional addr-spec.
  *
  */
case class ReturnPath(path: String) extends EmailHeaderField {
  def name: String = ReturnPath.name
}

object ReturnPath extends HeaderDescription[ReturnPath] {

  val name: String = "Return-Path"

  val codec: Codec[ReturnPath] = {
    scodec.codecs.utf8.exmap(
      s => {
        val s0 = s.trim
        if (s0.startsWith("<") && s0.endsWith(">")) {
          Attempt.successful(ReturnPath(s0.tail.init))
        } else Attempt.failure(Err(s"Path must be enclosed in <> brackets. : $s"))
      }
      , rp => Attempt.successful('<' + rp.path + '>')
    )
  }


  def fieldCodec: Codec[EmailHeaderField] = codec.upcast

}
