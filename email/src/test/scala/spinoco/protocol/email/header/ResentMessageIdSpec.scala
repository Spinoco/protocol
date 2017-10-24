package spinoco.protocol.email.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties
import shapeless.tag
/**
  * Created by pach on 23/10/17.
  */
object ResentMessageIdSpec  extends Properties("ResentMessageId") {

  import spinoco.protocol.email.SpecUtil._
  implicit val HeaderCodec = ResentMessageId.codec

  property("single-id") = protect {

    verify(
      "<123456@foo.com>"
      , ResentMessageId(tag[MessageId]("123456@foo.com"))
    )

  }


}
