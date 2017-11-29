package spinoco.protocol.http.header.value

import java.time.LocalDateTime

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import spinoco.protocol.common.util.attempt
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.common.codec._

import scala.concurrent.duration._
import scala.collection.immutable.HashSet

/**
  * see http://tools.ietf.org/html/rfc6265
  */
sealed case class HttpCookie(
   name: String
   , content: String
   , expires: Option[LocalDateTime]
   , maxAge: Option[FiniteDuration]
   , domain: Option[String]
   , path: Option[String]
   , secure: Boolean
   , httpOnly: Boolean
   , params: Map[String, String]
)

object HttpCookie {

  object rfc2616 {
    val controls: Seq[Byte] = (0 to 31).map(_.toByte) :+ 127.toByte
    val separators = Seq[Byte]('=', ')', '(', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '{', '}', ' ', 9.toByte)
    val nonTokens = HashSet.empty[Byte] ++ separators ++ controls

    val token: Codec[String] = takeWhile(asciiString) { b => nonTokens.contains(b) == false }
  }

  val cookiePair: Codec[(String,String)] =
    (rfc2616.token :: constant(_equal) :: (ignoreWS ~> ascii)).as[(String,String)]
  // https://tools.ietf.org/html/rfc6265#section-5.3

  val codec: Codec[HttpCookie] = {
    val kvTuple:Codec[(String,String)] = { choice (
        asciiConstant("secure").exmap ( _ => Attempt.successful("Secure" -> "") , { case (k,_)  => if (k == "Secure") Attempt.successful(()) else Attempt.failure(Err("Secure expected")) })
        , asciiConstant("httponly").exmap ( _ => Attempt.successful("HttpOnly" -> "") , { case (k,_)  => if (k == "HttpOnly") Attempt.successful(()) else Attempt.failure(Err("HttpOnly expected")) })
        , cookiePair
      )
    }

    val defaultParams = Set("Max-Age", "Expires", "Domain", "Path", "Secure", "HttpOnly")

    def decode(h: (String, String), t: List[(String, String)]):Attempt[HttpCookie] = {
      val params = t.toMap
      params.get("Max-Age").map { s => attempt(s.toLong.seconds).map(Some(_)) }.getOrElse(Attempt.successful(None)).flatMap { maxAge =>
      params.get("Expires").map { s => httpDateTimeCodec.decodeValue(BitVector.view(s.getBytes)).map(Some(_)) }.getOrElse(Attempt.successful(None)).map { expires =>
        HttpCookie(
          name = h._1
          , content = h._2
          , expires = expires
          , maxAge = maxAge
          , domain = params.get("Domain")
          , path = params.get("Path")
          , secure = params.isDefinedAt("Secure")
          , httpOnly = params.isDefinedAt("HttpOnly")
          , params = params -- defaultParams
        )
      }}

    }

    def encode(cookie:HttpCookie): Attempt[((String, String), List[(String, String)])] = {
      Attempt.successful {
        (cookie.name -> cookie.content) ->
        (cookie.params
          ++ cookie.expires.map { localDateTime2String  }.map { "Expires" -> _ } .toMap
          ++ cookie.maxAge.map { _.toSeconds.toString }.map { "Max-Age" -> _ }.toMap
          ++ cookie.domain.map { "Domain" -> _ }.toMap
          ++ cookie.path.map { "Path" -> _ }.toMap
          ++ (if (cookie.secure) Map("Secure" -> "true") else Map.empty)
          ++ (if (cookie.httpOnly) Map("HttpOnly" -> "true") else Map.empty)
        ).toList
      }
    }

    parametrizedN(semicolon, semicolon_SP, kvTuple, kvTuple).exmap(decode _ tupled,encode)
  }

}
