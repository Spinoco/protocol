package spinoco.protocol.http.header

import scodec.{Attempt, Err}
import spinoco.protocol.http.codec.helper._
import spinoco.protocol.http.header.value.{HeaderCodecDefinition, RangeUnit}

/**
  *   RFC 7233 section 2.3
  *
  *   @see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Ranges
  */
sealed case class `Accept-Ranges`(value: Option[RangeUnit.Bytes.type]) extends DefaultHeader


object `Accept-Ranges` { val codec =
  HeaderCodecDefinition[`Accept-Ranges`](
    trimmedAsciiString.exmap(
      {
        case "none" => Attempt.successful(`Accept-Ranges`(None))
        case "bytes" => Attempt.successful(`Accept-Ranges`(Some(RangeUnit.Bytes)))
        case other => Attempt.failure(Err(s"only none or bytes supported, got $other"))
      }
      , r => Attempt.successful(r.value.map(_ => "bytes").getOrElse("none"))
    )

  )
}