package spinoco.protocol.mail.header

import org.scalacheck.Prop.protect
import org.scalacheck.Properties

/**
  * Created by pach on 23/10/17.
  */
object ReturnPathSpec extends Properties("ReturnPath") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = `Return-Path`.codec


  property("single-email") = protect {

    verify(
      "<john.doe@spinoco.com>"
      , `Return-Path`("john.doe@spinoco.com")
    )

  }


}

