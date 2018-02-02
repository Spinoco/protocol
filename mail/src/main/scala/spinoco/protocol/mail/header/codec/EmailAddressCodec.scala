package spinoco.protocol.mail.header.codec

import scodec.bits.BitVector
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import spinoco.protocol.mail.EmailAddress

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
            // This is "quoted-string" as per RFC 2822
            // In RFC 2047 it is said that inside "quoted-string" there cannot be the "encoded-word"
            // But some clients do send it there, as such there is an deviation from RFC 2047 in terms of decoding.
            val quotesStrippedDisplay =
              if (display.length > 1 && display.head == '"' && display.last == '"') {
                display.init.tail
              } else {
                display
              }

            Attempt.fromEither(
              BitVector.encodeAscii(quotesStrippedDisplay)
              .left
              .map(err => Err(s"Could not encode display into ASCII for RFC2047 decoding due to: $err"))
            ).flatMap (RFC2047Codec.codec.decode).map{ result =>
              (addr, Some(result.value).map(_.trim).filter(_.nonEmpty))
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
          case Some(display) =>
            RFC2047Codec.codec.encode(display).flatMap{_.decodeAscii match {
              case Left(err) => Attempt.failure(Err(s"Could not encode display test for display $display due to $err"))
              case Right(succ) => Attempt.successful(succ + " <" + addr.address + ">")
            }}

        }
      }
    )



}
