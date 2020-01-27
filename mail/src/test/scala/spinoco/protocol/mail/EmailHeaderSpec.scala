package spinoco.protocol.mail

import java.time.{ZoneOffset, ZonedDateTime}

import org.scalacheck.Prop._
import org.scalacheck.Properties
import scodec.Attempt
import scodec.bits.ByteVector
import shapeless.tag
import spinoco.protocol.mail.header._
import spinoco.protocol.mail.header.codec.EmailHeaderCodec
import spinoco.protocol.mail.mime.TransferEncoding
import spinoco.protocol.mime.{ContentType, MIMECharset, MediaType}

/**
  * Created by pach on 23/10/17.
  */
object EmailHeaderSpec extends  Properties("EmailHeader") {

  property("decode-header") = protect {

    val email =
      """Delivered-To: pavel.chlupacek@spinoco.com
        |Received: by 10.223.153.210 with SMTP id y76csp1426558wrb;
        |        Fri, 20 Oct 2017 12:30:11 -0700 (PDT)
        |X-Google-Smtp-Source: ABhQp+Skhcbtb3N49Au5lRx/CwKjIiFmpM6tZCoZaOfwMlp+FutZJx2aYcnOcdGNRGhJRAZ3bwmt
        |X-Received: by 10.223.129.99 with SMTP id 90mr5147175wrm.243.1508527811866;
        |        Fri, 20 Oct 2017 12:30:11 -0700 (PDT)
        |ARC-Seal: i=1; a=rsa-sha256; t=1508527811; cv=none;
        |        d=google.com; s=arc-20160816;
        |        b=DT0H/IhVSsyNZUsQle/HrDCYspwy0WqWKOo6ko5PeehQZR9cTe/II1NhSEPomT2cxC
        |         8E7fXrZ71muu94mZG4UmrHyIoNbz5f4mCrdiA/tS4AX9WGwq/I12jmbcnb7ZJb2cxgJ0
        |         YJub7q+XgW4QQ4mztiCccAGRODMd2WQFgzTHXBBtP4yfnKGCjNwzmirPa6NSiaNfL8pr
        |         Fnt4MCpBYwRk1y14R2Usen4UX75YPhxgXqoV3pYUQ3+IQ+EJD1Za0um/06fhrKlhBN+2
        |         QnPfbIc7fSm5+RMXWExBwhSKE227dpN19IWEjXySkFdojm+rZ4NkIENh2uSm3X/YJ2Ef
        |         p3og==
        |ARC-Message-Signature: i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com; s=arc-20160816;
        |        h=content-transfer-encoding:subject:date:to:from:mime-version
        |         :feedback-id:list-unsubscribe:precedence:dkim-signature:message-id
        |         :arc-authentication-results;
        |        bh=7R3CwnHlEO/w9xwq7Maas/Jfku95Rg2P+3ISYDsE0YE=;
        |        b=pxj/y+Psux/6939Vn9O+Vpw+7FmOAU6BRqaMmM2blqTtPH8VuxpYZaSfBREBkxLju1
        |         ueIWsxtRu9TH6WgkX6rH4QNAwTpDT8TsjsmneRfS3OEpf6AADUaAX833qG8F2BISuyQN
        |         DWlwGnGdvCXBtVMVX03/L5e3gKRfzX/VAPMT6N3nMuSYtqTVxJBfsCU/iauHG68uwxyq
        |         V0gDqdo1I8dFgJF7YaV180YzCKMibWj3ALPYxQ6dhO4Ias7cTpVOn2aKDol5KVs9Spwc
        |         y8yhxFElV552l8WWIJqnfrf5oobVvZxYkV2Q3wzKDj+2kBnMrpUapvCnQCvtI/JJbMFT
        |         Y0iw==
        |ARC-Authentication-Results: i=1; mx.google.com;
        |       dkim=pass header.i=@info.alza.cz header.s=key01 header.b=Y48wVnAp;
        |       spf=pass (google.com: domain of webinfo@info.alza.cz designates 195.250.141.77 as permitted sender) smtp.mailfrom=webinfo@info.alza.cz;
        |       dmarc=pass (p=NONE sp=NONE dis=NONE) header.from=alza.cz
        |Return-Path: <webinfo@info.alza.cz>
        |Received: from mx47.alza.cz (mx47.alza.cz. [195.250.141.77])
        |        by mx.google.com with SMTP id v21si1222349wra.442.2017.10.20.12.30.11
        |        for <pavel.chlupacek@spinoco.com>;
        |        Fri, 20 Oct 2017 12:30:11 -0700 (PDT)
        |Received-SPF: pass (google.com: domain of webinfo@info.alza.cz designates 195.250.141.77 as permitted sender) client-ip=195.250.141.77;
        |Authentication-Results: mx.google.com;
        |       dkim=pass header.i=@info.alza.cz header.s=key01 header.b=Y48wVnAp;
        |       spf=pass (google.com: domain of webinfo@info.alza.cz designates 195.250.141.77 as permitted sender) smtp.mailfrom=webinfo@info.alza.cz;
        |       dmarc=pass (p=NONE sp=NONE dis=NONE) header.from=alza.cz
        |Message-ID: <59ea4ec3.558bdf0a.1f4fc.c8d9SMTPIN_ADDED_BROKEN@mx.google.com>
        |X-Google-Original-Message-ID: 57641.2670051.MLG55268@info.alza.cz
        |dkim-signature: v=1; a=rsa-sha1; d=info.alza.cz; s=key01;
        |	c=relaxed/relaxed; q=dns/txt; h=From:Subject:Date:Message-ID:To:MIME-Version:Content-Type:Content-Transfer-Encoding:List-Unsubscribe;
        |	bh=RflSsAmEVrBpirQwj62Hw1PDgko=;
        |	b=Y48wVnAp1RSxQyoGTSHDA7bq3+gOK1GINgtLWWr7pSUqjkZarDg14kozBAJIYMT2R5Gu8zb9W3vOcEoQ5aE0Hmbl0qz0tnOtP++ARsU+LA3XU+tLOw8W6s1RLruy2juk4KzBDPfVpEjNRDkNfHjBUfTqBzpCY7M+Y0BcTII0MS4=
        |Received: from SUPPORT4 (SUPPORT4.alz.lcl [192.168.231.84])
        |	by mx47.alza.cz with ESMTP
        |	; Fri, 20 Oct 2017 20:53:48 +0200
        |Precedence: bulk
        |List-Unsubscribe: <mailto:unsubscribe@alza.cz?subject=unsubscribe&body=%3cUserId%3e2670051%3c%2fUserId%3e%3cTypeId%3e139851%3c%2fTypeId%3e>
        |Feedback-ID: 57641:2670051:MLG55268:c43ac1209c
        |MIME-Version: 1.0
        |From: Alza.cz, Info <webinfo@info.alza.cz>
        |To: pavel.chlupacek@spinoco.com
        |Date: 20 Oct 2017 20:53:48 +0200
        |Subject: =?utf-8?B?UMWZZWQgdMSbbWlobGUga2xvYm91ayBkb2zFryE=?=
        |Content-Type: text/html; charset=utf-8
        |Content-Transfer-Encoding: base64
        |Auto-Submitted: auto-notified
        |
        |""".stripMargin.linesWithSeparators.map(_.stripLineEnd).mkString("\r\n")


    EmailHeaderCodec.codec(Int.MaxValue).decodeValue(ByteVector.view(email.getBytes).bits).map { header =>
       header ?= EmailHeader(
         List(
           GenericField("Delivered-To", "pavel.chlupacek@spinoco.com")
           , Received("by 10.223.153.210 with SMTP id y76csp1426558wrb", ZonedDateTime.of(2017, 10 ,20, 12, 30, 11, 0, ZoneOffset.ofHoursMinutes(-7,0)))
           , GenericField("X-Google-Smtp-Source", "ABhQp+Skhcbtb3N49Au5lRx/CwKjIiFmpM6tZCoZaOfwMlp+FutZJx2aYcnOcdGNRGhJRAZ3bwmt")
           , GenericField("X-Received", "by 10.223.129.99 with SMTP id 90mr5147175wrm.243.1508527811866;        Fri, 20 Oct 2017 12:30:11 -0700 (PDT)")
           , GenericField("ARC-Seal", "i=1; a=rsa-sha256; t=1508527811; cv=none;        d=google.com; s=arc-20160816;        b=DT0H/IhVSsyNZUsQle/HrDCYspwy0WqWKOo6ko5PeehQZR9cTe/II1NhSEPomT2cxC         8E7fXrZ71muu94mZG4UmrHyIoNbz5f4mCrdiA/tS4AX9WGwq/I12jmbcnb7ZJb2cxgJ0         YJub7q+XgW4QQ4mztiCccAGRODMd2WQFgzTHXBBtP4yfnKGCjNwzmirPa6NSiaNfL8pr         Fnt4MCpBYwRk1y14R2Usen4UX75YPhxgXqoV3pYUQ3+IQ+EJD1Za0um/06fhrKlhBN+2         QnPfbIc7fSm5+RMXWExBwhSKE227dpN19IWEjXySkFdojm+rZ4NkIENh2uSm3X/YJ2Ef         p3og==")
           , GenericField("ARC-Message-Signature", "i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com; s=arc-20160816;        h=content-transfer-encoding:subject:date:to:from:mime-version         :feedback-id:list-unsubscribe:precedence:dkim-signature:message-id         :arc-authentication-results;        bh=7R3CwnHlEO/w9xwq7Maas/Jfku95Rg2P+3ISYDsE0YE=;        b=pxj/y+Psux/6939Vn9O+Vpw+7FmOAU6BRqaMmM2blqTtPH8VuxpYZaSfBREBkxLju1         ueIWsxtRu9TH6WgkX6rH4QNAwTpDT8TsjsmneRfS3OEpf6AADUaAX833qG8F2BISuyQN         DWlwGnGdvCXBtVMVX03/L5e3gKRfzX/VAPMT6N3nMuSYtqTVxJBfsCU/iauHG68uwxyq         V0gDqdo1I8dFgJF7YaV180YzCKMibWj3ALPYxQ6dhO4Ias7cTpVOn2aKDol5KVs9Spwc         y8yhxFElV552l8WWIJqnfrf5oobVvZxYkV2Q3wzKDj+2kBnMrpUapvCnQCvtI/JJbMFT         Y0iw==")
           , GenericField("ARC-Authentication-Results", "i=1; mx.google.com;       dkim=pass header.i=@info.alza.cz header.s=key01 header.b=Y48wVnAp;       spf=pass (google.com: domain of webinfo@info.alza.cz designates 195.250.141.77 as permitted sender) smtp.mailfrom=webinfo@info.alza.cz;       dmarc=pass (p=NONE sp=NONE dis=NONE) header.from=alza.cz")
           , `Return-Path`("webinfo@info.alza.cz")
           , Received("from mx47.alza.cz (mx47.alza.cz. [195.250.141.77])        by mx.google.com with SMTP id v21si1222349wra.442.2017.10.20.12.30.11        for <pavel.chlupacek@spinoco.com>", ZonedDateTime.of(2017, 10 ,20, 12, 30, 11, 0, ZoneOffset.ofHoursMinutes(-7,0)))
           , GenericField("Received-SPF", "pass (google.com: domain of webinfo@info.alza.cz designates 195.250.141.77 as permitted sender) client-ip=195.250.141.77;")
           , GenericField("Authentication-Results", "mx.google.com;       dkim=pass header.i=@info.alza.cz header.s=key01 header.b=Y48wVnAp;       spf=pass (google.com: domain of webinfo@info.alza.cz designates 195.250.141.77 as permitted sender) smtp.mailfrom=webinfo@info.alza.cz;       dmarc=pass (p=NONE sp=NONE dis=NONE) header.from=alza.cz")
           , `Message-ID`(tag[`Message-ID`]("59ea4ec3.558bdf0a.1f4fc.c8d9SMTPIN_ADDED_BROKEN@mx.google.com"))
           , GenericField("X-Google-Original-Message-ID", "57641.2670051.MLG55268@info.alza.cz")
           , GenericField("dkim-signature", "v=1; a=rsa-sha1; d=info.alza.cz; s=key01;\tc=relaxed/relaxed; q=dns/txt; h=From:Subject:Date:Message-ID:To:MIME-Version:Content-Type:Content-Transfer-Encoding:List-Unsubscribe;\tbh=RflSsAmEVrBpirQwj62Hw1PDgko=;\tb=Y48wVnAp1RSxQyoGTSHDA7bq3+gOK1GINgtLWWr7pSUqjkZarDg14kozBAJIYMT2R5Gu8zb9W3vOcEoQ5aE0Hmbl0qz0tnOtP++ARsU+LA3XU+tLOw8W6s1RLruy2juk4KzBDPfVpEjNRDkNfHjBUfTqBzpCY7M+Y0BcTII0MS4=")
           , Received("from SUPPORT4 (SUPPORT4.alz.lcl [192.168.231.84])\tby mx47.alza.cz with ESMTP\t", ZonedDateTime.of(2017, 10 ,20, 20, 53, 48, 0, ZoneOffset.ofHoursMinutes(2,0)))
           , GenericField("Precedence", "bulk")
           , GenericField("List-Unsubscribe", "<mailto:unsubscribe@alza.cz?subject=unsubscribe&body=%3cUserId%3e2670051%3c%2fUserId%3e%3cTypeId%3e139851%3c%2fTypeId%3e>")
           , GenericField("Feedback-ID", "57641:2670051:MLG55268:c43ac1209c")
           , `MIME-Version`("1.0")
           , From(EmailAddress("webinfo","info.alza.cz",Some("Alza.cz, Info")),List())
           , Destination(DestinationType.To, EmailAddress("pavel.chlupacek","spinoco.com",None), Nil)
           , OriginationDate(ZonedDateTime.of(2017, 10 ,20, 20, 53, 48, 0, ZoneOffset.ofHoursMinutes(2,0)))
           , Subject("Před těmihle klobouk dolů!")
           , `Content-Type`(ContentType.TextContent(MediaType.`text/html`, Some(MIMECharset.`UTF-8`)))
           , `Content-Transfer-Encoding`(TransferEncoding.Base64)
           , `Auto-Submitted`(`Auto-Submitted`.AutoType.AutoNotified, None)
         )
       )
    }.fold(err => { println(s"FAILED: $err"); falsified }, identity)

  }

  property("encode-header") = protect {
    val header = EmailHeader(
      List(
        Received("by 10.223.153.210 with SMTP id y76csp1426558wrb", ZonedDateTime.of(2017, 10 ,20, 12, 30, 11, 0, ZoneOffset.ofHoursMinutes(-7,0)))
        , `Return-Path`("webinfo@info.alza.cz")
        , `Message-ID`(tag[`Message-ID`]("59ea4ec3.558bdf0a.1f4fc.c8d9SMTPIN_ADDED_BROKEN@mx.google.com"))
        , `MIME-Version`("1.0")
        , From(EmailAddress("webinfo","info.alza.cz",Some("Alza.cz, Info")),List())
        , Destination(DestinationType.To, EmailAddress("pavel.chlupacek","spinoco.com",None), Nil)
        , OriginationDate(ZonedDateTime.of(2017, 10 ,20, 20, 53, 48, 0, ZoneOffset.ofHoursMinutes(2,0)))
        , Subject("Před těmihle klobouk dolů!")
        , `Content-Type`(ContentType.TextContent(MediaType.`text/html`, Some(MIMECharset.`UTF-8`)))
        , `Content-Transfer-Encoding`(TransferEncoding.Base64)
      )
    )


    val encoded = EmailHeaderCodec.codec(Int.MaxValue).encode(header)


    val expect = ByteVector.view(
                 """Received: by 10.223.153.210 with SMTP id y76csp1426558wrb;
                   | Fri, 20 Oct 2017 12:30:11 -0700
                   |Return-Path: <webinfo@info.alza.cz>
                   |Message-ID: <59ea4ec3.558bdf0a.1f4fc.c8d9SMTPIN_ADDED_BROKEN@mx.google.com>
                   |MIME-Version: 1.0
                   |From: "Alza.cz, Info" <webinfo@info.alza.cz>
                   |To: pavel.chlupacek@spinoco.com
                   |Date: Fri, 20 Oct 2017 20:53:48 +0200
                   |Subject: =?UTF-8?Q?P=C5=99ed_t=C4=9Bmihle_klobouk_dol=C5=AF!?=
                   |Content-Type: text/html; charset=utf-8
                   |Content-Transfer-Encoding: base64
                   |
                   |""".stripMargin.linesWithSeparators.map(_.stripLineEnd).mkString("\r\n").getBytes)


    encoded ?= Attempt.successful(expect.bits)

  }

}
