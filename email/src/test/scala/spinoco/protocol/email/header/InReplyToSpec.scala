package spinoco.protocol.email.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import shapeless.tag
/**
  * Created by pach on 20/10/17.
  */
object InReplyToSpec extends Properties("InReplyTo") {

  import spinoco.protocol.email.SpecUtil._
  implicit val HeaderCodec = InReplyTo.codec

  property("single-id") = protect {

    verify(
      "<123456@foo.com>"
      , InReplyTo(tag[MessageId]("123456@foo.com"), Nil)
    )

  }


  property("multiple-ids") = protect {

    verify(
      "<123456@foo.com>\r\n " +
      "<12345678@foo.com> <123456789@foo.com> \r\n    <12345678AB@foo.com> "
      , InReplyTo(tag[MessageId]("123456@foo.com"), List(
        tag[MessageId]("12345678@foo.com")
        , tag[MessageId]("123456789@foo.com")
        , tag[MessageId]("12345678AB@foo.com")
      ))
      , "<123456@foo.com>\r\n " +
        "<12345678@foo.com>\r\n " +
        "<123456789@foo.com>\r\n " +
        "<12345678AB@foo.com>"
    )

  }

}
