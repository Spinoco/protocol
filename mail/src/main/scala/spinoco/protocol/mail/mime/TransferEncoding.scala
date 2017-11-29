package spinoco.protocol.mail.mime

import scodec.{Attempt, Codec}
import scodec.codecs._
import spinoco.protocol.common.util.attempt


trait TransferEncoding

object TransferEncoding {

  val Bits7: StandardEncoding = StandardEncoding(DefaultEncodings.Bits7)
  val Bits8: StandardEncoding = StandardEncoding(DefaultEncodings.Bits8)
  val Binary: StandardEncoding = StandardEncoding(DefaultEncodings.Binary)
  val QuotedPrintable: StandardEncoding = StandardEncoding(DefaultEncodings.QuotedPrintable)
  val Base64: StandardEncoding = StandardEncoding(DefaultEncodings.Base64)


  case class StandardEncoding(encoding: DefaultEncodings.Value) extends TransferEncoding
  case class XEncoding(encoding: String) extends TransferEncoding

  object DefaultEncodings extends Enumeration {
    val Bits7 = Value("7bit")
    val Bits8 = Value("8bit")
    val Binary = Value("binary")
    val QuotedPrintable = Value("quoted-printable")
    val Base64 = Value("base64")
  }

  val codec: Codec[TransferEncoding] = {
    ascii.narrow(
      s => attempt(StandardEncoding(DefaultEncodings.withName(s.toLowerCase))) orElse Attempt.successful(XEncoding(s))
      , {
        case StandardEncoding(enc) => enc.toString
        case XEncoding(enc) => enc
      }
    )
  }

}
