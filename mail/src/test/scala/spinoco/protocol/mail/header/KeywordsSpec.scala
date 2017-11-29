package spinoco.protocol.mail.header

import org.scalacheck.Prop._
import org.scalacheck.Properties


object KeywordsSpec extends Properties("Keywords") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = Keywords.codec

  property("single") = protect {
    verify(
      "the-topics"
      , Keywords("the-topics", Nil)
    )

  }

  property("single-quoted") = protect {
    verify(
      "\"the topics\""
      , Keywords("the topics", Nil)
    )

  }


  property("multiple") = protect {
    verify(
      "\"the topics\",the-topics,third"
      , Keywords("the topics", List("the-topics", "third"))
    )

  }

}
