package spinoco.protocol.email.header

import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.tag

/**
  * Created by pach on 19/10/17.
  */
object MessageIdSpec  extends Properties("MessageId") {

  import spinoco.protocol.email.SpecUtil._
  implicit val HeaderCodec = MessageId.codec

  property("single-id") = protect {

    verify(
      "<123456@foo.com>"
      , MessageId(tag[MessageId]("123456@foo.com"))
    )

  }


}
