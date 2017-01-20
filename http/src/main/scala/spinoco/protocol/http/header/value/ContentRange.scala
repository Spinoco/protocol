package spinoco.protocol.http.header.value

/**
  * Created by pach on 12/01/17.
  */
sealed case class ContentRange(rangeUnit: RangeUnit, range: ContentRange.Range)

object ContentRange {

  sealed trait Range

  sealed case class Default(first: Long, last: Long) extends Range
  sealed case class Unsatisfiable(length: Option[Long]) extends Range

}
