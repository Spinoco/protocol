package spinoco.protocol.sdp

import java.time.LocalDateTime

import scala.concurrent.duration.FiniteDuration

/**
  * Created by pach on 04/03/17.
  */
case class TimeZone(
  adjustment: LocalDateTime
  , zoneOffset: FiniteDuration
)
