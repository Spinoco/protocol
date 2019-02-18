package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.common.codec._
import spinoco.protocol.common.Terminator
import spinoco.protocol.http.codec.helper._

sealed case class HttpChallenge(
  scheme: HttpChallengeScheme.Value
  , params: List[(String, String)]
)

object HttpChallenge {

  val codec: Codec[HttpChallenge] = {
    val parameter:Codec[(String,String)] = {
      terminated(trimmedAsciiToken, Terminator.constantString1("=")) ~ eventuallyQuotedUTF8String
    }

    bytesWsRemoved.codedAs(
      bytesUntil(! _.toChar.isWhitespace).xmap[ByteVector](identity, bv => bv ++ SP).codedAs(HttpChallengeScheme.codec) ~
        bytesWsRemoved.codedAs(scodec.codecs.choice(
        commaDelimited(parameter)
        , delimitedBy(SP, comma_SP, eventuallyQuotedUTF8String).xmap[List[(String, String)]](
          split => {
            split.lift(0).map("realm" -> _).toList ++
            split.lift(1).map("error" -> _).toList ++
            split.lift(2).map("error_description" -> _).toList
          }
          , list => list.map{ case (k, v) => s"$k=$v"}
        )
      ))
    ).xmap(HttpChallenge.apply _ tupled, ch => ch.scheme -> ch.params)

  }

}


object HttpChallengeScheme extends Enumeration {
  val Basic = Value("Basic")
  val Digest = Value("Digest")
  val OAuth = Value("OAuth")

  val codec: Codec[HttpChallengeScheme.Value] = {
    import scodec.Attempt._
    import spinoco.protocol.common.util._
    trimmedAsciiToken.exmap(
      s => attempt(HttpChallengeScheme.withName(s))
      , s => successful(s.toString)
    )
  }
}