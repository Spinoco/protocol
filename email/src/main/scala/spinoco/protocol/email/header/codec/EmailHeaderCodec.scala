package spinoco.protocol.email.header.codec

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import spinoco.protocol.email.EmailHeader
import spinoco.protocol.email.header._

import scala.annotation.tailrec

/**
  * Created by pach on 23/10/17.
  */
object EmailHeaderCodec {

  /**
    * Creates a header codec accoring to specified parameteres
    * @param maxHeaderLength  Max length of the header. If header is larger than this value, then the encoding // decoding will fail.
    * @param otherHeaders     Apart from default headers, theses supplied headers may be used when decoeing // encoding. For exmaple
    *                         supply any custom headers here.
    * @return
    */
  def codec(maxHeaderLength: Int, otherHeaders: (String, Codec[EmailHeaderField]) *): Codec[EmailHeader] = {
    val allHeaders = defaultHeaders ++ otherHeaders.map { case (k, c) => (k.toLowerCase, c) }.toMap

    val commaSpace = ByteVector.view(": ".getBytes).bits

    val fieldHeaderCodec =
      new Codec[EmailHeaderField] {
        def encode(value: EmailHeaderField): Attempt[BitVector] = {
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

        def decode(bits: BitVector): Attempt[DecodeResult[EmailHeaderField]] = {
          val bytes = bits.bytes
          val headerNameBytes = bytes.takeWhile(_ != ':')
          scodec.codecs.utf8.decode(headerNameBytes.bits) flatMap { case DecodeResult(headerName, _) =>
            val codec = allHeaders.get(headerName.toLowerCase) match {
              case Some(c) => c
              case None => scodec.codecs.utf8.xmap[GenericField](s => GenericField(headerName, s), _.value).upcast[EmailHeaderField]
            }

            codec.decode(impl.trimHead(bytes.drop(headerNameBytes.length + 1)).bits)
          }
        }
      }

      impl.emailHeaderFieldsCodec(fieldHeaderCodec).xmap (
        flds => EmailHeader(flds)
        , h => h.fields
      )
  }

  val defaultHeaders: Map[String, Codec[EmailHeaderField]] = {
    Seq(
      Comments
      , Destination.To
      , Destination.Cc
      , Destination.Bcc
      , From
      , InReplyTo
      , Keywords
      , MessageId
      , OriginationDate
      , Received
      , References
      , ReplyTo
      , ResentDate
      , ResentDestination.To
      , ResentDestination.Cc
      , ResentDestination.Bcc
      , ResentFrom
      , ResentMessageId
      , ResentSender
      , ReturnPath
      , Sender
      , Subject
    ).map { hdr => (hdr.name.toLowerCase , hdr.fieldCodec) }.toMap
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
    def emailHeaderFieldsCodec(codec: Codec[EmailHeaderField]): Codec[List[EmailHeaderField]] = {
      val CRLFBytes = ByteVector.view("\r\n".getBytes)
      val CRLF = CRLFBytes.bits


      new Codec[List[EmailHeaderField]] {
        def encode(value: List[EmailHeaderField]): Attempt[BitVector] = {
          @tailrec
          def go(rem: List[EmailHeaderField], acc: BitVector): Attempt[BitVector] = {
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

        def decode(bits: BitVector): Attempt[DecodeResult[List[EmailHeaderField]]] = {
          @tailrec
          def go(rem: ByteVector, fwsAcc: ByteVector, acc: Vector[EmailHeaderField]): Attempt[List[EmailHeaderField]] = {
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
