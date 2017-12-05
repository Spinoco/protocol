package spinoco.protocol.email.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties

/**
  * Created by pach on 23/10/17.
  */
object ReturnPathSpec extends Properties("ReturnPath") {

  import spinoco.protocol.email.SpecUtil._
  implicit val HeaderCodec = ReturnPath.codec


  property("single-email") = protect {

    verify(
      "<john.doe@spinoco.com>"
      , ReturnPath("john.doe@spinoco.com")
    )

  }


}

