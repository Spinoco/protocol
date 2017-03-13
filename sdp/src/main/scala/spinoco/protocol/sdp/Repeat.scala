package spinoco.protocol.sdp

import scala.concurrent.duration.FiniteDuration

/**
  * Created by pach on 04/03/17.
  */
case class Repeat(
  repeat: FiniteDuration
  , active: FiniteDuration
  , offsets: Vector[FiniteDuration]
)
