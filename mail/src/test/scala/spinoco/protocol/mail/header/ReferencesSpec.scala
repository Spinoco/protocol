package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import shapeless.tag

/**
  * Created by pach on 20/10/17.
  */
object ReferencesSpec  extends Properties("References") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = References.codec

  property("single-id") = protect {

    verify(
      "<123456@foo.com>"
      , References(tag[`Message-ID`]("123456@foo.com"), Nil)
    )

  }


  property("multiple-ids") = protect {
    verify(
      "<123456@foo.com>\r\n " +
        "<12345678@foo.com> <123456789@foo.com> \r\n    <12345678AB@foo.com> "
      , References(tag[`Message-ID`]("123456@foo.com"), List(
        tag[`Message-ID`]("12345678@foo.com")
        , tag[`Message-ID`]("123456789@foo.com")
        , tag[`Message-ID`]("12345678AB@foo.com")
      ))
      , "<123456@foo.com>\r\n " +
        "<12345678@foo.com>\r\n " +
        "<123456789@foo.com>\r\n " +
        "<12345678AB@foo.com>"
    )
  }

  property("multiple-ids-comma-separated") = protect {
    verify(
      ""
      , References(tag[`Message-ID`]("123456@foo.com"), List(
        tag[`Message-ID`]("12345678@foo.com")
        , tag[`Message-ID`]("123456789@foo.com")
        , tag[`Message-ID`]("12345678AB@foo.com")
      ))
      , "<123456@foo.com>\r\n " +
        "<12345678@foo.com>\r\n " +
        "<123456789@foo.com>\r\n " +
        "<12345678AB@foo.com>"
    )
  }

}
