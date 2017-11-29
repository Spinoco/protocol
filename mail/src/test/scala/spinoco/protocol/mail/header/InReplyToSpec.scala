package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import shapeless.tag
/**
  * Created by pach on 20/10/17.
  */
object InReplyToSpec extends Properties("InReplyTo") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = `In-Reply-To`.codec

  property("single-id") = protect {

    verify(
      "<123456@foo.com>"
      , `In-Reply-To`(tag[`Message-ID`]("123456@foo.com"), Nil)
    )

  }


  property("multiple-ids") = protect {

    verify(
      "<123456@foo.com>\r\n " +
      "<12345678@foo.com> <123456789@foo.com> \r\n    <12345678AB@foo.com> "
      , `In-Reply-To`(tag[`Message-ID`]("123456@foo.com"), List(
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
