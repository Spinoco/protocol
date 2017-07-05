package spinoco.protocol.http.header

import java.time.LocalDateTime

import scodec.{Attempt, Err}
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.HeaderCodecDefinition
import spinoco.protocol.common.codec._
import scodec.codecs._


/**
  *   RFC 7231 section 5.3
  *   @see  https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Expires
  */
sealed case class Expires(value: Either[Int, LocalDateTime]) extends DefaultHeader


object Expires {  val codec =
  HeaderCodecDefinition[Expires](
    choice(
      httpDateTimeCodec.exmap (
        dt => Attempt.Successful(Expires(Right(dt)))
        , {
          case Expires(Left(_)) => Attempt.failure(Err("No date, int supplied"))
          case Expires(Right(dt)) => Attempt.successful(dt)
        })
      , intAsString.exmap(
        int => Attempt.Successful(Expires(Left(int)))
        , {
          case Expires(Left(int)) => Attempt.successful(int)
          case Expires(Right(dt)) => Attempt.failure(Err("No date, or int supplied"))
        }
      )
      , asciiString.unit("0").xmap(_ => Expires(Left(0)), _ => ())
    )

  )
}