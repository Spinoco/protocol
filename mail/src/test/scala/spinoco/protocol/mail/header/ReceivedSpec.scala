package spinoco.protocol.mail.header

import java.time.{ZoneId, ZoneOffset, ZonedDateTime}

import org.scalacheck.Prop.protect
import org.scalacheck.Properties

/**
  * Created by pach on 23/10/17.
  */
object ReceivedSpec extends Properties("Received") {

  import spinoco.protocol.mail.SpecUtil._
  implicit val HeaderCodec = Received.codec

  property("single-line") = protect {
    verify(
      "by 172.16.136.49 with SMTP id aybv6k1v6crbrx8bpifvm57u0m4pf5xa2t7nqpxn; Wed, 14 Feb 2018 14:05:10 GMT"
      , Received("by 172.16.136.49 with SMTP id aybv6k1v6crbrx8bpifvm57u0m4pf5xa2t7nqpxn", ZonedDateTime.of(2018, 2, 14, 14, 5, 10, 0, ZoneId.of("GMT")))
      , "by 172.16.136.49 with SMTP id aybv6k1v6crbrx8bpifvm57u0m4pf5xa2t7nqpxn;\r\n Wed, 14 Feb 2018 14:05:10 +0000"
    )

  }


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

  property("single-entry.non-rfc") = protect {

    verify(
      "by filter0527p1iad2.sendgrid.net with SMTP id filter0527p1iad2-22563-5A8AD67A-5 2018-02-19 13:51:54.319416103 +0000 UTC"
      , Received("by filter0527p1iad2.sendgrid.net with SMTP id filter0527p1iad2-22563-5A8AD67A-5", ZonedDateTime.of(2018, 2, 19, 13, 51, 54, 319416103, ZoneId.of("UTC")))
      , "by filter0527p1iad2.sendgrid.net with SMTP id filter0527p1iad2-22563-5A8AD67A-5;\r\n Mon, 19 Feb 2018 13:51:54 +0000"
    )

  }

  property("single-entry.non-rfc-date") = protect {

    verify(
      "from 87b8d379c4a2 (ec2-34-197-179-31.compute-1.amazonaws.com [34.197.179.31]) by ismtpd0002p1iad2.sendgrid.net (SG) with ESMTP id lXvmPQVwTx-1lTzLQtKl7g for <milan.raulim@spinoco.com>; Mon, 19 Feb 2018 22:00:14.807 +0000 (UTC)"
      , Received("from 87b8d379c4a2 (ec2-34-197-179-31.compute-1.amazonaws.com [34.197.179.31]) by ismtpd0002p1iad2.sendgrid.net (SG) with ESMTP id lXvmPQVwTx-1lTzLQtKl7g for <milan.raulim@spinoco.com>", ZonedDateTime.of(2018, 2, 19, 22, 0, 14, 807, ZoneOffset.UTC))
      , "from 87b8d379c4a2 (ec2-34-197-179-31.compute-1.amazonaws.com [34.197.179.31]) by ismtpd0002p1iad2.sendgrid.net (SG) with ESMTP id lXvmPQVwTx-1lTzLQtKl7g for <milan.raulim@spinoco.com>;\r\n Mon, 19 Feb 2018 22:00:14 +0000"
    )

  }

  property("single-entry.non-rfc-date2") = protect {
    verify(
      "from MTgyMzE0NA (ec2-52-21-133-138.compute-1.amazonaws.com [52.21.133.138]) by ismtpd0008p1iad2.sendgrid.net (SG) with HTTP id 3mkK_06IRJaQNoC4tdWWpg Tue, 20 Feb 2018 05:11:56.362 +0000 (UTC)"
      , Received("from MTgyMzE0NA (ec2-52-21-133-138.compute-1.amazonaws.com [52.21.133.138]) by ismtpd0008p1iad2.sendgrid.net (SG) with HTTP id 3mkK_06IRJaQNoC4tdWWpg", ZonedDateTime.of(2018, 2, 20, 5, 11, 56, 362, ZoneOffset.UTC))
      , "from MTgyMzE0NA (ec2-52-21-133-138.compute-1.amazonaws.com [52.21.133.138]) by ismtpd0008p1iad2.sendgrid.net (SG) with HTTP id 3mkK_06IRJaQNoC4tdWWpg;\r\n Tue, 20 Feb 2018 05:11:56 +0000"
    )

  }
}
