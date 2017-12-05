package spinoco.protocol.email.header.codec

import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import spinoco.protocol.email.EmailAddress

/**
  * Codec for email address
  */
object EmailAddressCodec {

  val codec: Codec[EmailAddress] =
    ascii.exmap(
      s0 => {
        val s = s0.trim
        val open = s.indexOf("<")
        val addressAndDisplay =
        if (open >= 0) {
          // parse address with display
          val end = s.indexOf(">", open)
          if (end > 0) {
            val addr = s.slice(open+1, end)

            // display may be quoted, in that case just take quoted part as is,
            // otherwise decode non ascii
            val display = s.take(open).dropRight(1).trim
            if (display.length > 1 && display.head == '"' && display.last == '"') {
              Attempt.successful((addr, Some(display.init.tail)))
            } else {
              decodeNonAscii(s.take(open).dropRight(1)) map { decoded =>
                (addr, Some(decoded).map(_.trim).filter(_.nonEmpty))
              }
            }
          } else {
            Attempt.failure(Err(s"Invalid Email format, missing end of email address indicator(>): $s"))
          }
        } else {
          Attempt.successful((s, None))
        }

        addressAndDisplay flatMap { case (address, display) =>
          val parts = address.split("@")
          if (parts.length != 2) Attempt.failure(Err(s"Address string must be local@domain format: $s"))
          else Attempt.successful(EmailAddress(parts.head, parts.last, display))

        }
      }
      , addr => {
        addr.display match {
          case None => Attempt.successful(addr.address)
          case Some(display) => Attempt.successful(encodeNonAscii(display) + " <" + addr.address + ">")
        }
      }
    )



}
