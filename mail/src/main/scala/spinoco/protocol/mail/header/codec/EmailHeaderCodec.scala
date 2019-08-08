package spinoco.protocol.mail.header.codec

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import shapeless.Typeable
import spinoco.protocol.mail.EmailHeader
import spinoco.protocol.mail.header._
import spinoco.protocol.mail.mime.MIMEHeader

import scala.annotation.tailrec


object EmailHeaderCodec {

  /**
    * Creates a header codec accoring to specified parameteres
    * @param maxHeaderLength  Max length of the header. If header is larger than this value, then the encoding // decoding will fail.
    * @param otherHeaders     Apart from default headers, theses supplied headers may be used when decoeing // encoding. For exmaple
    *                         supply any custom headers here.
    * @return
    */
  def codec(maxHeaderLength: Int, otherHeaders: (String, Codec[EmailHeaderField]) *): Codec[EmailHeader] = {
    fieldCodec(maxHeaderLength, defaultEmailHeaders ++ otherHeaders.map { case (k, c) => (k.toLowerCase, c) }.toMap)
    .xmap(EmailHeader.apply, _.fields)
  }

  def mimeCodec(maxHeaderLength: Int, otherHeaders: (String, Codec[ContentHeaderField]) *): Codec[MIMEHeader] = {
    fieldCodec(maxHeaderLength, defaultMIMEHeaders ++ otherHeaders.map { case (k, c) => (k.toLowerCase, c) }.toMap)
    .xmap(MIMEHeader.apply, _.fields)
  }


  /**
    * Codec for header fields in the email.
    * @param maxHeaderLength    Max length of the header
    * @param allHeaders         All supported header fields not listed here will be decoded as GenericField
    * @tparam H
    * @return
    */
  def fieldCodec[H <: EmailHeaderField](maxHeaderLength: Int, allHeaders: Map[String, Codec[H]])(implicit T: Typeable[H]): Codec[List[H]] = {

    val commaSpace = ByteVector.view(": ".getBytes).bits

    val fieldHeaderCodec =
      new Codec[H] {
        def encode(value: H): Attempt[BitVector] = {
          allHeaders.get(value.name.toLowerCase) match {
            case Some(codec) =>
              codec.encode(value) map { bits =>
                ByteVector.view(value.name.getBytes).bits ++ commaSpace ++ bits
              }

            case None =>
              value match {
                case GenericField(name, v) =>
                  Attempt.successful(ByteVector.view(name.getBytes).bits ++ commaSpace ++ ByteVector.view(v.getBytes).bits)

                case other =>
                  Attempt.failure(Err(s"Unsupported header: $other"))
              }
          }
        }

        def sizeBound: SizeBound = SizeBound.unknown

        def decode(bits: BitVector): Attempt[DecodeResult[H]] = {
          val bytes = bits.bytes
          val headerNameBytes = bytes.takeWhile(_ != ':')
          scodec.codecs.utf8.decode(headerNameBytes.bits) flatMap { case DecodeResult(headerName, _) =>

            val bits = impl.trimHead(bytes.drop(headerNameBytes.length + 1)).bits
            allHeaders.get(headerName.toLowerCase) match {
              case Some(c) => c.decode(bits) orElse (scodec.codecs.utf8.decode(bits) orElse scodec.codecs.ascii.decode(bits)).flatMap { rslt =>
                T.cast(NonRFC(headerName, rslt.value)) match {
                  case Some(h) => Attempt.successful(rslt.map(_ => h))
                  case None => Attempt.failure(Err(s"Cannot encode as Non RFC, hence ${T.describe} is not superclass of it"))
                }
              }

              case None => (scodec.codecs.utf8.decode(bits) orElse scodec.codecs.ascii.decode(bits)).flatMap { rslt =>
                T.cast(GenericField(headerName, rslt.value)) match {
                  case Some(h) => Attempt.successful(rslt.map(_ => h))
                  case None => Attempt.failure(Err(s"Cannot encode as Generic Field, hence ${T.describe} is not superclass of it"))
                }
              }
            }

          }
        }
      }

      impl.emailHeaderFieldsCodec(fieldHeaderCodec)
  }

  /** Headers used in Email **/
  val defaultEmailHeaders: Map[String, Codec[EmailHeaderField]] = {
    Seq(
      `Auto-Submitted`
      , Comments
      , Destination.To
      , Destination.Cc
      , Destination.Bcc
      , Destination.ReplyTo
      , From
      , `In-Reply-To`
      , Keywords
      , `Message-ID`
      , OriginationDate
      , Received
      , References
      , `Resent-Date`
      , ResentDestination.To
      , ResentDestination.Cc
      , ResentDestination.Bcc
      , `Resent-From`
      , `Resent-Message-ID`
      , `Resent-Sender`
      , `Return-Path`
      , Sender
      , Subject
      , `Content-Type`
      , `Content-Transfer-Encoding`
      , `Content-Description`
      , `Content-Disposition`
      , `Content-ID`
      , `MIME-Version`
    ).map { hdr => (hdr.name.toLowerCase , hdr.emailHeaderField) }.toMap
  }

  /** headers used im MIME Parts **/
  val defaultMIMEHeaders: Map[String, Codec[ContentHeaderField]] = {
    Seq(
       `Content-Type`
      , `Content-Transfer-Encoding`
      , `Content-Description`
      , `Content-Disposition`
      , `Content-ID`
    ).map { hdr => (hdr.name.toLowerCase , hdr.contentHeaderField) }.toMap
  }

  object impl {

    /** strips any whitespace from start of the bytes supplied **/
    def trimHead(bytes: ByteVector): ByteVector = {
      bytes.dropWhile(_.toChar.isWhitespace)
    }


    /**
     * Decodes email headers, so the bytes are split by CRLF (if not part of FWS) and then, for every line the `codec`
     * is applied to decode header field.
     * @param codec Codec to decode header fields
     */
    def emailHeaderFieldsCodec[H <: EmailHeaderField](codec: Codec[H]): Codec[List[H]] = {
      val CRLFBytes = ByteVector.view("\r\n".getBytes)
      val CRLF = CRLFBytes.bits


      new Codec[List[H]] {
        def encode(value: List[H]): Attempt[BitVector] = {
          @tailrec
          def go(rem: List[H], acc: BitVector): Attempt[BitVector] = {
            rem.headOption match {
              case Some(hf) =>
                codec.encode(hf) match {
                  case Attempt.Successful(bits) => go(rem.tail, acc ++ bits ++ CRLF)
                  case Attempt.Failure(err) => Attempt.failure(err)
                }

              case None =>
                Attempt.successful(acc)
            }
          }

          go(value, BitVector.empty)
        }

        def sizeBound: SizeBound = SizeBound.unknown

        def decode(bits: BitVector): Attempt[DecodeResult[List[H]]] = {
          @tailrec
          def go(rem: ByteVector, fwsAcc: ByteVector, acc: Vector[H]): Attempt[List[H]] = {
            if (rem.isEmpty) {
              Attempt.successful(acc.toList)
            } else {
              val crlfIdx = rem.indexOfSlice(CRLFBytes)
              if (crlfIdx < 0) Attempt.failure(Err(s"Every header field must be terminated with CRLF: ${rem.decodeUtf8} of ${bits.bytes}"))
              else {
                val (chunk, next) = rem.splitAt(crlfIdx)
                if (next.size > 2 && next(2).toChar.isWhitespace) {
                  // FWS
                  go(next.drop(2), fwsAcc ++ chunk, acc)
                } else {
                  codec.decodeValue((fwsAcc ++ chunk).bits) match {
                    case Attempt.Successful(field) => go(next.drop(2), ByteVector.empty, acc :+ field)
                    case Attempt.Failure(err) => Attempt.failure(Err(s"Failed to decode header: ${(fwsAcc ++ chunk).decodeUtf8} : $err"))
                  }
                }
              }
            }
          }

          go(bits.bytes, ByteVector.empty, Vector.empty) map { flds =>
            DecodeResult(flds, BitVector.empty)
          }
        }
      }
    }

  }

}
