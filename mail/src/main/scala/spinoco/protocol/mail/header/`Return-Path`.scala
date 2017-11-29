package spinoco.protocol.mail.header

import scodec.{Attempt, Codec, Err}

/**
  * RFC 5322 3.6.7:
  *
  * The "Return-Path:" header field contains a pair of angle brackets
  *  that enclose an optional addr-spec.
  *
  */
case class `Return-Path`(path: String) extends DefaultEmailHeaderField

object `Return-Path` extends DefaultHeaderDescription[`Return-Path`] {

  val codec: Codec[`Return-Path`] = {
    scodec.codecs.utf8.exmap(
      s => {
        val s0 = s.trim
        if (s0.startsWith("<") && s0.endsWith(">")) {
          Attempt.successful(`Return-Path`(s0.tail.init))
        } else Attempt.failure(Err(s"Path must be enclosed in <> brackets. : $s"))
      }
      , rp => Attempt.successful('<' + rp.path + '>')
    )
  }

}
