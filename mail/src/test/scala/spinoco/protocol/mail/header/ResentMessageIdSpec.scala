package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import shapeless.tag
/**
  * Created by pach on 23/10/17.
  */
object ResentMessageIdSpec  extends Properties("ResentMessageId") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = `Resent-Message-ID`.codec

  property("single-id") = protect {

    verify(
      "<123456@foo.com>"
      , `Resent-Message-ID`(tag[`Message-ID`]("123456@foo.com"))
    )

  }


}
