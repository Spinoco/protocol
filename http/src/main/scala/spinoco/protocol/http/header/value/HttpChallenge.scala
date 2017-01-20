package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.common.codec._
import spinoco.protocol.http.codec.helper._

sealed case class HttpChallenge(
  scheme: HttpChallengeScheme.Value
  , params: List[(String, String)]
)

object HttpChallenge {

  val codec: Codec[HttpChallenge] = {
    val parameter:Codec[(String,String)] = {
      tuple[String,String](_equal, trimmedAsciiString, quotedString)
    }

    bytesWsRemoved.codedAs(
      bytesUntil(! _.toChar.isWhitespace).xmap[ByteVector](identity, bv => bv ++ SP).codedAs(HttpChallengeScheme.codec) ~
      bytesWsRemoved.codedAs(commaDelimited(parameter))
    ).xmap(HttpChallenge.apply _ tupled, ch => ch.scheme -> ch.params)

  }

}


object HttpChallengeScheme extends Enumeration {
  val Basic = Value("Basic")
  val Digest = Value("Digest")

  val codec: Codec[HttpChallengeScheme.Value] = {
    import scodec.Attempt._
    import spinoco.protocol.common.util._
    trimmedAsciiString.exmap(
      s => attempt(HttpChallengeScheme.withName(s))
      , s => successful(s.toString)
    )
  }
}