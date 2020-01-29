package spinoco.protocol.mail.header.codec

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import spinoco.protocol.common.codec._
import spinoco.protocol.mail.EmailAddress

/**
  * Codec for email address
  */
object EmailAddressCodec {

  val codec: Codec[EmailAddress] = {

    val emailAddress = (ignoreWS ~> choice(dotAtomString, quotedString)) ~ (constantString1("@") ~> (dotAtomString <~ ignoreWS))
    val bracketAddress = constantString1("<") ~> emailAddress <~ constantString1(">")

    val quotedDisplay = (choice(dotAtomString, quotedString) <~ WSP).widen[Option[String]](
      Some(_).map(_.trim).filter(_.nonEmpty)
      , Attempt.fromOption(_, Err("Failed to create email address without display segment"))
    )

    val displayCodec = takeWhile(ascii)(_ != '<').widen[Option[String]](
      Some(_).map(_.trim).filter(_.nonEmpty)
      , Attempt.fromOption(_, Err("Failed to create email address without display segment"))
    )

    choice(
      quotedDisplay ~ bracketAddress
      , emailAddress.xmap[(Option[String], (String, String))](None -> _ , _._2)
      , bracketAddress.xmap[(Option[String], (String, String))](None -> _ , _._2)
      , displayCodec ~ bracketAddress
    ).exmap[EmailAddress]({ case (display, (localPart, domain)) =>
      display match {
        case None => Attempt.successful(EmailAddress(localPart, domain, None))

        case Some(d) =>
          Attempt.fromEither(
            BitVector.encodeAscii(d).left.map(err => Err(s"Could not encode display into ASCII for RFC2047 decoding due to: $err"))
          ).flatMap(RFC2047Codec.codec.decode).flatMap { result =>
            Attempt.successful(EmailAddress(localPart, domain, Some(result.value.trim).filter(_.nonEmpty)))
          }
        }
    }, { case EmailAddress(localPart, domain, display) =>
      display match {
        case None =>  Attempt.successful((display, (localPart, domain)))

        case Some(d) => RFC2047Codec.codec.encode(d).flatMap {
          _.decodeAscii match {
            case Left(err) => Attempt.failure(Err(s"Could not encode display test for display $d due to $err"))
            case Right(succ) => Attempt.successful((Some(succ.trim).filter(_.nonEmpty), (localPart, domain)))
          }
        }
      }
    })

  }

}
