package spinoco.protocol.sdp

import java.time.{LocalDateTime, ZoneOffset}

/**
  * Created by pach on 04/03/17.
  */
case class Timing(start:LocalDateTime, stop: LocalDateTime)

object Timing {

  val startOfEpoch = LocalDateTime.ofEpochSecond(-2208988800L, 0, ZoneOffset.UTC)

}
