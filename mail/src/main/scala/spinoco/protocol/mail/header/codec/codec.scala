package spinoco.protocol.mail.header

import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.codecs._
import shapeless.tag
import shapeless.tag.@@
import spinoco.protocol.common.codec._

import scala.annotation.tailrec

/**
  * Created by pach on 17/10/17.
  */
package object codec {

  private[codec] val `,`: ByteVector = ByteVector.view(",".getBytes)
  private[codec] val `, `: ByteVector = ByteVector.view(", ".getBytes)
  private[codec] val foldingComma: ByteVector = ByteVector.view(",\r\n ".getBytes)
  private[codec] val cfws : ByteVector = ByteVector.view("\r\n ".getBytes)


  /**
    * Creates a codec, that decodes list of comma separated values.
    *
    * There must be at least one `A` decoded for this to succeed
    *
    * @param fold If true, then folding whitespace is inserted while encoding.
    */
  def commaSeparated[A](codec: Codec[A], fold: Boolean): Codec[(A, List[A])] = {
    val encodeDelimiter = if (fold) foldingComma else `,`
    delimitedBy(`,`, encodeDelimiter, codec) exmap(
      la => {
        if (la.isEmpty) Attempt.failure(Err("No element provided, expecting one"))
        else Attempt.successful((la.head, la.tail))
      }
      , { case (a, la) => Attempt.successful(a +: la) }
    )
  }

  /**
    *  Decodes `A` where the individual items are separated by folding whitespace as per RFC.
    *  Encodes by inserting `\r\n ` (CFWS) between individual items.
    *
    *  Individual `A` encodings may not contain whitespace character.
    *
    * @param codec
    * @tparam A
    * @return
    */
  def cfwsSeparated[A](codec: Codec[A]): Codec[(A, List[A])] = {

    val listCodec = listDelimited(cfws.bits, codec)

    new Codec[(A, List[A])] {
      def decode(bits: BitVector): Attempt[DecodeResult[(A, List[A])]] = {

        @tailrec
        def go(bytes: ByteVector, acc: Vector[A]): Attempt[Vector[A]] = {
          val start = bytes.dropWhile { b => b.toChar.isWhitespace }
          if (start.isEmpty) Attempt.successful(acc)
          else {
            val toDecode = start.takeWhile { b => !b.toChar.isWhitespace }
            val next = start.drop(toDecode.length).dropWhile { b => b.toChar.isWhitespace }
            codec.decode(toDecode.bits) match {
              case Attempt.Successful(rslt) =>
                if (rslt.remainder.nonEmpty) Attempt.failure(Err(s"Decoded successfully, but remainder was not processed: $rslt"))
                else go(next, acc :+ rslt.value)
              case Attempt.Failure(err) => Attempt.failure(err)
            }
          }
        }

        go(bits.bytes, Vector.empty) flatMap { decoded =>
          if (decoded.isEmpty) Attempt.failure(Err("Expected at least one `A` got none"))
          else Attempt.successful(DecodeResult((decoded.head, decoded.tail.toList), BitVector.empty))
        }

      }

      def encode(value: (A, List[A])): Attempt[BitVector] =
        listCodec.encode(value._1 +: value._2)

      def sizeBound: SizeBound =
        SizeBound.unknown
    }
  }


  private val AtomAcceptChars: Set[Char] = Set(
    '!', '#', '$', '%', '&', ''', '*', '+', '-', '/', '=', '?', '^', '_', '`', '{', '}', '|', '~'
  )

  def isAtomChar(c: Char): Boolean =
    c.isLetterOrDigit || AtomAcceptChars.contains(c)

  def isAtomDotChar(c: Char): Boolean =
    c.isLetterOrDigit || AtomAcceptChars.contains(c) || c == '.'


  /** string that is encoded as atom **/
  val atomString: Codec[String] = {
    def verifyAtom(s: String): Attempt[String] = {
      if (s.forall(isAtomChar)) Attempt.successful(s)
      else Attempt.failure(Err(s"String is not atom: $s"))
    }
    utf8.exmap(verifyAtom, verifyAtom)
  }

  /** quoted string **/
  val quotedString: Codec[String] = {
    utf8.exmap(
      s => {
        val s0 = s.trim
        if (s0.size > 1 && s0.head == '"' && s0.last == '"') {
          Attempt.successful(s0.init.tail)
        } else Attempt.failure(Err(s"Expected Quoted string, got: $s"))
      }
      , s => Attempt.successful('"' + s + '"')
    )
  }

  // keyword in RFC 5322
  val keyword = choice(atomString, quotedString)


  val msgIdCodec: Codec[String @@ `Message-ID`] = {
    def decode(s0: String): Attempt[String @@ `Message-ID`] = {
      val s = s0.trim
      if (s.length > 1 && s.head == '<' && s.last == '>' ) {
        val content = s.tail.init
        val leftRight = content.split('@')
        if (leftRight.length != 2) Attempt.failure(Err(s"Invalid message d: $s0"))
        else {
          if (leftRight(0).exists(c => !isAtomDotChar(c)) || leftRight(1).exists(c => !isAtomDotChar(c))) Attempt.failure(Err(s"Invalid message id, id is not atomchar per RFC: $content"))
          else Attempt.successful(tag[`Message-ID`](leftRight(0) +"@" + leftRight(1)))
        }
      } else Attempt.failure(Err(s"Invalid message Id: $s0"))
    }

    def encode(id: String @@ `Message-ID`): Attempt[String] =
      Attempt.successful("<" + id + ">")

    ascii.exmap (decode, encode)
  }





}
