package spinoco.protocol.http.header.value

import scodec.bits.ByteVector
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.common.Terminator
import spinoco.protocol.http.codec.helper._


/**
  * Created by pach on 12/01/17.
  */
sealed trait  HttpCredentials


object HttpCredentials {

  sealed case class BasicHttpCredentials(username: String, password: String) extends HttpCredentials

  sealed case class OAuthToken(tpe: String, token: String) extends HttpCredentials

  sealed case class DigestHttpCredentials(tpe: String, params: Map[String, String]) extends HttpCredentials

  val bearerCodec: Codec[OAuthToken] =
    (utf8Token ~ (whitespace() ~> utf8String)).xmap(
      { case (tpe, token) => OAuthToken(tpe, token)}
      , oAuth => oAuth.tpe -> oAuth.token
    )

  val basicCodec: Codec[BasicHttpCredentials]  = {
    (asciiConstant("Basic") ~> (whitespace() ~> base64Encoded)).exmap(
      { token =>
        Attempt.fromEither(token.decodeUtf8.left.map(ex => Err(s"Invalid character encoding: ${ex.getMessage}"))).flatMap { s =>
          val split = s.split(':')
          if (split.size != 2) Attempt.failure(Err(s"Expected username:pass but got $s"))
          else Attempt.successful(BasicHttpCredentials(split(0), split(1)))
        }
      }
      , { h => Attempt.successful(ByteVector.view((h.username + ":" + h.password).getBytes)) }
    )
  }

  val digestCodec: Codec[DigestHttpCredentials] = {
    ((ignoreWS ~> asciiToken) ~ (whitespace() ~> commaDelimited(
      terminated(trimmedAsciiToken, Terminator.constantString1("=")) ~ httpMaybeQuotedUTF8String
    ))).xmap(
      { case (digest, params) => DigestHttpCredentials(digest, params.toMap) }
      , { h => (h.tpe, h.params.toList) }
    )
  }

  val codec : Codec[HttpCredentials] = choice(
    basicCodec.upcast[HttpCredentials]
    , digestCodec.upcast[HttpCredentials]
    , bearerCodec.upcast[HttpCredentials]
  )

}
