package spinoco.protocol.mail.header

import java.time.{ZoneOffset, ZonedDateTime}

import org.scalacheck.Prop.protect
import org.scalacheck.Properties

/**
  * Created by pach on 23/10/17.
  */
object ReceivedSpec extends Properties("Received") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = Received.codec

  property("single-entry") = protect {

    verify(
      "by 10.223.153.210 with SMTP id y76csp1426558wrb;\r\n        Fri, 20 Oct 2017 12:30:11 -0700 (PDT)"
      , Received("by 10.223.153.210 with SMTP id y76csp1426558wrb", ZonedDateTime.of(2017, 10, 20, 12, 30, 11, 0, ZoneOffset.ofHoursMinutes(-7,0)))
      , "by 10.223.153.210 with SMTP id y76csp1426558wrb;\r\n Fri, 20 Oct 2017 12:30:11 -0700"
    )

  }

  property("single-entry.day.single-digit") = protect {

    verify(
      "by 10.100.247.137 with SMTP id v9csp7671429pjk;\r\n        Thu, 1 Feb 2018 01:11:57 -0800 (PST)"
      , Received("by 10.100.247.137 with SMTP id v9csp7671429pjk", ZonedDateTime.of(2018, 2, 1, 1, 11, 57, 0, ZoneOffset.ofHoursMinutes(-8,0)))
      , "by 10.100.247.137 with SMTP id v9csp7671429pjk;\r\n Thu, 1 Feb 2018 01:11:57 -0800"
    )

  }

}
