package spinoco.protocol.http.header.value

import scodec.Codec

import scala.concurrent.duration._

/**
  * Created by pach on 12/01/17.
  */
sealed case class CacheDirective(name:String, time:Option[FiniteDuration])


object CacheDirective {


  val `no-cache` = CacheDirective ("no-cache", None)
  val `no-store` = CacheDirective ("no-store", None)
  val `no-transform` = CacheDirective ("no-transform", None)
  def `max-age`(time: FiniteDuration): CacheDirective = CacheDirective("max-age",Some(time))
  def `max-stale`(time: Option[FiniteDuration]): CacheDirective = CacheDirective("max-stale", time)
  def `min-fresh`(time: FiniteDuration): CacheDirective = CacheDirective("max-fresh",Some(time))
  val `only-if-cached` = CacheDirective ("no-cache", None)
  val `public` = CacheDirective ("public", None)
  val `private`  = CacheDirective ("private", None)
  val `must-revalidate` = CacheDirective ("must-revalidate", None)
  val `proxy-revalidate` = CacheDirective ("proxy-revalidate", None)
  def `s-maxage`(time: FiniteDuration): CacheDirective = CacheDirective("s-maxage",Some(time))


  val codec : Codec[CacheDirective] = {
    import spinoco.protocol.common.codec._
    import spinoco.protocol.http.codec.helper._

    parametrized(_equal, trimmedAsciiString, intAsString)
    .xmap(
      { case (name, time) => CacheDirective(name.toLowerCase, time.map(_.seconds)) }
      , cd => cd.name -> cd.time.map(_.toSeconds.toInt)
    )
  }

}
