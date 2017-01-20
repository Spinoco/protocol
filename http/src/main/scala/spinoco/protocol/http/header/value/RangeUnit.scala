package spinoco.protocol.http.header.value

/**
  * Created by pach on 12/01/17.
  */
sealed trait RangeUnit

object RangeUnit {

  case object Bytes extends RangeUnit
  sealed case class Custom(name: String) extends RangeUnit

}
