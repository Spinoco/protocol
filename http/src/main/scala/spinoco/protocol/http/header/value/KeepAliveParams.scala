package spinoco.protocol.http.header.value

import scodec.{Attempt, Codec, Err}
import spinoco.protocol.common.util._
import spinoco.protocol.http.codec.helper._

import scala.concurrent.duration._


sealed case class KeepAliveParams( timeout: FiniteDuration, maxConn: Int)


object KeepAliveParams {


  val codec: Codec[KeepAliveParams] = {

    def decode(pairs:List[(String, String)]):Attempt[KeepAliveParams] = {
      def getTimeout: Attempt[FiniteDuration] = {
        pairs.collectFirst({ case (k, v) if k.toLowerCase == "timeout" =>
          attempt { v.toInt.seconds }
        }).getOrElse(Attempt.failure(Err("Timeout parameter is missing")))
      }

      def getMax: Attempt[Int] = {
        pairs.collectFirst({ case (k, v) if k.toLowerCase == "max" =>
          attempt { v.toInt }
        }).getOrElse(Attempt.failure(Err("Max parameter is missing")))
      }

      for {
        timeout <- getTimeout
        max <- getMax
      } yield KeepAliveParams(timeout, max)
    }

    def encode(params: KeepAliveParams): Attempt[List[(String, String)]] = {
      Attempt.successful(List(
        "timeout" -> params.timeout.toSeconds.toString
        , "max" -> params.maxConn.toString
      ))
    }

    commaDelimitedMin(tuple(_equal, trimmedAsciiString, trimmedAsciiString), 2).exmap(decode,encode)
  }

}