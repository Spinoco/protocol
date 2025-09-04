package spinoco.protocol.mail.header

import scodec.Codec

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
    scodec.codecs.utf8.xmap(
      s => {
        val s0 = s.trim
        if (s0.startsWith("<") && s0.endsWith(">")) {
          `Return-Path`(s0.tail.init)
        } else {
          `Return-Path`(s0.trim)
        }
      }
      , rp => s"<${rp.path}>"
    )
  }

}
