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


  property("non-rfc.single-email") = protect {
    verify(
      "bce-se_n.7237.318.716502_milanraulim-gmail.com@se-acc-7237.se-bounce-0001.cz"
      , `Return-Path`("bce-se_n.7237.318.716502_milanraulim-gmail.com@se-acc-7237.se-bounce-0001.cz")
      , "<bce-se_n.7237.318.716502_milanraulim-gmail.com@se-acc-7237.se-bounce-0001.cz>"
    )
  }

}

