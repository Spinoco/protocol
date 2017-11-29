package spinoco.protocol.mail.header

import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.tag

/**
  * Created by pach on 19/10/17.
  */
object MessageIdSpec  extends Properties("MessageId") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = `Message-ID`.codec

  property("single-id") = protect {

    verify(
      "<123456@foo.com>"
      , `Message-ID`(tag[`Message-ID`]("123456@foo.com"))
    )

  }


}
