package spinoco.protocol.mail.imap.codec

import java.time.LocalDate

import org.scalacheck.Prop._
import org.scalacheck.Properties
import scodec.Attempt
import scodec.bits.BitVector
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.imap.BodyStructure._

object IMAPBodyPartCodecSpec extends Properties("IMAPBodyPartCodec") {

//  property("simple-body-structure") = protect {
//
//    IMAPBodyPartCodec.bodyStructure.decodeValue(BitVector.view(
//      """(BODYSTRUCTURE (("TEXT" "HTML" ("CHARSET" "UTF-8") NIL NIL "QUOTED-PRINTABLE" 22621 598 NIL ("INLINE" NIL) NIL)("IMAGE" "PNG" ("NAME" "page.word-icon.png") "<page.word-icon>" NIL "BASE64" 328 NIL ("INLINE" ("FILENAME" "page.word-icon.png")) NIL)("IMAGE" "PNG" ("NAME" "footer-desktop-logo.png") "<footer-desktop-logo>" NIL "BASE64" 1182 NIL ("INLINE" ("FILENAME" "footer-desktop-logo.png")) NIL)("IMAGE" "PNG" ("NAME" "footer-mobile-logo.png") "<footer-mobile-logo>" NIL "BASE64" 524 NIL ("INLINE" ("FILENAME" "footer-mobile-logo.png")) NIL) "RELATED" ("BOUNDARY" "----=_Part_11768_185568052.1510095660090") NIL NIL))""".getBytes
//    )) ?= Attempt.successful(
//      MultiBodyPart(
//        parts = Vector(
//          SingleBodyPart(
//            tpe = BodyTypeText(
//              subType = "HTML"
//              , fields = BodyFields(
//                params = Vector(("CHARSET", "UTF-8"))
//                , id = None
//                , desc = None
//                , encoding = "QUOTED-PRINTABLE"
//                , size = 22621
//              )
//              , lines = 598
//            )
//            , ext = Some(SingleBodyExtension(
//              md5 = None
//              , dsp = Some(("INLINE", Vector.empty))
//              , lang = Some(List())
//              , loc = None
//              , extensions = Vector()
//            ))
//          )
//          , SingleBodyPart(
//            tpe = BodyTypeBasic(
//              media = BasicMedia("IMAGE", "PNG")
//              , fields = BodyFields(
//                params = Vector(("NAME", "page.word-icon.png"))
//                , id = Some("<page.word-icon>")
//                , desc = None
//                , encoding = "BASE64"
//                , size = 328
//              )
//            )
//            , ext = Some(SingleBodyExtension(
//              md5 = None
//              , dsp = Some(("INLINE", Vector(("FILENAME", "page.word-icon.png"))))
//              , lang = Some(List())
//              , loc = None
//              , extensions = Vector()
//            ))
//          )
//          , SingleBodyPart(
//            tpe = BodyTypeBasic(
//              media = BasicMedia("IMAGE", "PNG")
//              , fields = BodyFields(
//                params = Vector(("NAME", "footer-desktop-logo.png"))
//                , id = Some("<footer-desktop-logo>")
//                , desc = None
//                , encoding = "BASE64"
//                , size = 1182
//              )
//            )
//            , ext = Some(SingleBodyExtension(
//              md5 = None
//              , dsp = Some(("INLINE", Vector(("FILENAME", "footer-desktop-logo.png"))))
//              , lang = Some(List())
//              , loc = None
//              , extensions = Vector()
//            ))
//          )
//          , SingleBodyPart(
//            tpe = BodyTypeBasic(
//              media = BasicMedia("IMAGE", "PNG")
//              , fields = BodyFields(
//                params = Vector(("NAME", "footer-mobile-logo.png"))
//                , id = Some("<footer-mobile-logo>")
//                , desc = None
//                , encoding = "BASE64"
//                , size = 524
//              )
//            )
//            , ext = Some(SingleBodyExtension(
//              md5 = None
//              , dsp = Some(("INLINE", Vector(("FILENAME", "footer-mobile-logo.png"))))
//              , lang = Some(List())
//              , loc = None
//              , extensions = Vector()
//            ))
//          )
//        )
//        , mediaSubType = "RELATED"
//        , ext = Some(MultiBodyExtension(
//          params = Vector(("BOUNDARY", "----=_Part_11768_185568052.1510095660090"))
//          , dsp = None
//          , lang = Some(Nil)
//          , loc = None
//          , extensions = Vector.empty
//        ))
//      )
//    )
//
//  }
//
//
//  property("simple-body-structure.RFC822") = protect {
//    IMAPBodyPartCodec.bodyStructure.decodeValue(BitVector.view(
//      """(BODYSTRUCTURE (((("TEXT" "PLAIN" ("CHARSET" "UTF-8") NIL NIL "7BIT" 483 10 NIL NIL NIL)("TEXT" "HTML" ("CHARSET" "UTF-8") NIL NIL "7BIT" 1745 35 NIL NIL NIL) "ALTERNATIVE" ("BOUNDARY" "001a114697ae45d6000565298052") NIL NIL)("IMAGE" "PNG" ("NAME" "icon.png") "<icon.png>" NIL "BASE64" 1986 NIL ("ATTACHMENT" ("FILENAME" "icon.png")) NIL) "RELATED" ("BOUNDARY" "001a114697ae45d5f40565298051") NIL NIL)("MESSAGE" "DELIVERY-STATUS" NIL NIL NIL "7BIT" 609 NIL NIL NIL)("MESSAGE" "RFC822" NIL NIL NIL "7BIT" 3292 ("Wed, 14 Feb 2018 11:23:22 +0100" "=?UTF-8?Q?[User_Notification]:_Milan_Raul=C3=ADm?=" (("=?UTF-8?Q?Milan_Raul=C3=ADm?=" NIL "zakaznik" "spinoco.com")) (("=?UTF-8?Q?Milan_Raul=C3=ADm?=" NIL "zakaznik" "spinoco.com")) ((NIL NIL "zakaznik" "spinoco.com")) ((NIL NIL "kayakoadmin" "spinoco.com")) NIL NIL NIL "<1518603802.5a840e1a1e5cc@zakaznik.spinoco.com>") ("TEXT" "HTML" ("CHARSET" "utf-8") NIL NIL "QUOTED-PRINTABLE" 789 16 NIL NIL NIL) 66 NIL NIL NIL) "REPORT" ("BOUNDARY" "001a114697ae45cdb30565298050" "REPORT-TYPE" "delivery-status") NIL NIL))""".getBytes
//    )) ?= Attempt.successful(
//      MultiBodyPart(
//        Vector(
//          MultiBodyPart(
//            parts = Vector(
//              MultiBodyPart(
//                parts = Vector(
//                  SingleBodyPart(
//                    BodyTypeText("PLAIN", BodyFields(Vector(("CHARSET", "UTF-8")), None, None, "7BIT", 483), 10)
//                    , Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
//                  )
//                  , SingleBodyPart(
//                    BodyTypeText("HTML", BodyFields(Vector(("CHARSET", "UTF-8")), None, None, "7BIT", 1745), 35),
//                    Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
//                  )
//                )
//                , "ALTERNATIVE"
//                , Some(MultiBodyExtension(Vector(("BOUNDARY", "001a114697ae45d6000565298052")), None, Some(List()), None, Vector()))
//              )
//              , SingleBodyPart(
//                BodyTypeBasic(BasicMedia("IMAGE", "PNG"), BodyFields(Vector(("NAME", "icon.png")), Some("<icon.png>"), None, "BASE64", 1986))
//                , Some(SingleBodyExtension(None, Some(("ATTACHMENT", Vector(("FILENAME", "icon.png")))), Some(List()), None, Vector()))
//              )
//            )
//            , "RELATED"
//            , Some(MultiBodyExtension(Vector(("BOUNDARY", "001a114697ae45d5f40565298051")), None, Some(List()), None, Vector()))
//          )
//          , SingleBodyPart(
//            BodyTypeBasic(BasicMedia("MESSAGE", "DELIVERY-STATUS"), BodyFields(Vector(), None, None, "7BIT", 609))
//            , Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
//          )
//          , SingleBodyPart(
//            BodyTypeMessage(
//              BodyFields(Vector(), None, None, "7BIT", 3292)
//              , Envelope(
//                LocalDate.of(2018, 2, 14)
//                , Some("=?UTF-8?Q?[User_Notification]:_Milan_Raul=C3=ADm?=")
//                , Vector(EmailAddress("zakaznik", "spinoco.com", Some("=?UTF-8?Q?Milan_Raul=C3=ADm?=")))
//                , Vector(EmailAddress("zakaznik", "spinoco.com", Some("=?UTF-8?Q?Milan_Raul=C3=ADm?=")))
//                , Vector(EmailAddress("zakaznik", "spinoco.com", None))
//                , Vector(EmailAddress("kayakoadmin", "spinoco.com", None))
//                , Vector(), Vector(), None, Some("<1518603802.5a840e1a1e5cc@zakaznik.spinoco.com>")
//              )
//              , SingleBodyPart(
//                BodyTypeText("HTML", BodyFields(Vector(("CHARSET", "utf-8")), None, None, "QUOTED-PRINTABLE", 789), 16)
//                , Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
//              ), 66
//            )
//            , Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
//          )
//        )
//        , "REPORT"
//        , Some(
//          MultiBodyExtension(
//            Vector(("BOUNDARY", "001a114697ae45cdb30565298050"), ("REPORT-TYPE", "delivery-status"))
//            , None
//            , Some(List())
//            , None
//            , Vector()
//          )
//        )
//      )
//    )
//
//  }
//
//  property("multi-body-extension") = protect {
//    IMAPBodyPartCodec.bodyStructure.decodeValue(BitVector.view(
//      """(BODYSTRUCTURE (("text" "plain" ("charset" "utf-8") NIL NIL "quoted-printable" 413 19 NIL NIL NIL NIL)("text" "html" ("charset" "utf-8") NIL NIL "quoted-printable" 731 10 NIL NIL NIL NIL) "alternative" ("boundary" "---0f38efd9-d1a7-49f3-acd9-31d7a496f37a---b14d35c6-7966-4a17-9fcd-28248679d25b---") NIL NIL NIL NIL))""".getBytes
//    )) ?= Attempt.successful(
//      MultiBodyPart(
//        parts = Vector(
//          SingleBodyPart(
//            tpe = BodyTypeText(
//              subType = "plain"
//              , fields = BodyFields(
//                params = Vector("charset" -> "utf-8")
//                , id = None
//                , desc = None
//                , encoding = "quoted-printable"
//                , size = 413
//              )
//              , lines = 19
//            )
//            , ext = Some(SingleBodyExtension(None, None, Some(List()), None, Vector.empty))
//          )
//          , SingleBodyPart(
//            tpe = BodyTypeText(
//              subType = "html"
//              , fields = BodyFields(
//                params = Vector("charset" -> "utf-8")
//                , id = None
//                , desc = None
//                , encoding = "quoted-printable"
//                , size = 731
//              )
//              , lines = 10
//            )
//            , ext = Some(SingleBodyExtension(None, None, Some(List()), None, Vector.empty))
//          )
//        )
//        , mediaSubType = "alternative"
//        , ext =  Some(MultiBodyExtension(
//          params = Vector("boundary" -> "---0f38efd9-d1a7-49f3-acd9-31d7a496f37a---b14d35c6-7966-4a17-9fcd-28248679d25b---")
//          , dsp = None
//          , lang = Some(List())
//          , loc = None
//          , extensions = Vector(ListBodyExtension(Vector.empty))
//        ))
//      )
//    )
//  }
//
//  property("simple-body-structure.lowercase") = protect {
//    IMAPBodyPartCodec.bodyStructure.decodeValue(BitVector.view(
//      """(BODYSTRUCTURE (("text" "html" ("charset" "utf-8") NIL NIL "quoted-printable" 22621 598 nil ("inline" nil) nil)("image" "png" ("name" "page.word-icon.png") "<page.word-icon>" nil "base64" 328 NIL ("inline" ("filename" "page.word-icon.png")) nil)("image" "png" ("name" "footer-desktop-logo.png") "<footer-desktop-logo>" nil "base64" 1182 nil ("inline" ("filename" "footer-desktop-logo.png")) nil)("image" "png" ("name" "footer-mobile-logo.png") "<footer-mobile-logo>" nil "base64" 524 nil ("inline" ("filename" "footer-mobile-logo.png")) nil) "related" ("boundary" "----=_Part_11768_185568052.1510095660090") nil nil))""".getBytes
//    )) ?= Attempt.successful(
//      MultiBodyPart(
//        parts = Vector(
//          SingleBodyPart(
//            tpe = BodyTypeText(
//              subType = "html"
//              , fields = BodyFields(
//                params = Vector(("charset", "utf-8"))
//                , id = None
//                , desc = None
//                , encoding = "quoted-printable"
//                , size = 22621
//              )
//              , lines = 598
//            )
//            , ext = Some(SingleBodyExtension(
//              md5 = None
//              , dsp = Some(("inline", Vector.empty))
//              , lang = Some(List())
//              , loc = None
//              , extensions = Vector()
//            ))
//          )
//          , SingleBodyPart(
//            tpe = BodyTypeBasic(
//              media = BasicMedia("image", "png")
//              , fields = BodyFields(
//                params = Vector(("name", "page.word-icon.png"))
//                , id = Some("<page.word-icon>")
//                , desc = None
//                , encoding = "base64"
//                , size = 328
//              )
//            )
//            , ext = Some(SingleBodyExtension(
//              md5 = None
//              , dsp = Some(("inline", Vector(("filename", "page.word-icon.png"))))
//              , lang = Some(List())
//              , loc = None
//              , extensions = Vector()
//            ))
//          )
//          , SingleBodyPart(
//            tpe = BodyTypeBasic(
//              media = BasicMedia("image", "png")
//              , fields = BodyFields(
//                params = Vector(("name", "footer-desktop-logo.png"))
//                , id = Some("<footer-desktop-logo>")
//                , desc = None
//                , encoding = "base64"
//                , size = 1182
//              )
//            )
//            , ext = Some(SingleBodyExtension(
//              md5 = None
//              , dsp = Some(("inline", Vector(("filename", "footer-desktop-logo.png"))))
//              , lang = Some(List())
//              , loc = None
//              , extensions = Vector()
//            ))
//          )
//          , SingleBodyPart(
//            tpe = BodyTypeBasic(
//              media = BasicMedia("image", "png")
//              , fields = BodyFields(
//                params = Vector(("name", "footer-mobile-logo.png"))
//                , id = Some("<footer-mobile-logo>")
//                , desc = None
//                , encoding = "base64"
//                , size = 524
//              )
//            )
//            , ext = Some(SingleBodyExtension(
//              md5 = None
//              , dsp = Some(("inline", Vector(("filename", "footer-mobile-logo.png"))))
//              , lang = Some(List())
//              , loc = None
//              , extensions = Vector()
//            ))
//          )
//        )
//        , mediaSubType = "related"
//        , ext = Some(MultiBodyExtension(
//          params = Vector(("boundary", "----=_Part_11768_185568052.1510095660090"))
//          , dsp = None
//          , lang = Some(Nil)
//          , loc = None
//          , extensions = Vector.empty
//        ))
//      )
//    )
//
//  }
//


  property("simple-body-structure.RFC822") = protect {
    IMAPBodyPartCodec.bodyStructure.decodeValue(BitVector.view(
      """(BODYSTRUCTURE (("text" "plain" ("charset" "utf-8" "format" "flowed") NIL NIL "8bit" 63 6 NIL NIL NIL NIL)("text" "html" ("charset" "utf-8") NIL NIL "8bit" 600 21 NIL NIL NIL NIL) "alternative" ("boundary" "------------6ADC3590299BEA6A994E5EFB") NIL "en-US") UID 31)""".getBytes
//      """(BODYSTRUCTURE (((("TEXT" "PLAIN" ("CHARSET" "UTF-8") NIL NIL "7BIT" 483 10 NIL NIL NIL)("TEXT" "HTML" ("CHARSET" "UTF-8") NIL NIL "7BIT" 1745 35 NIL NIL NIL) "ALTERNATIVE" ("BOUNDARY" "001a114697ae45d6000565298052") NIL NIL)("IMAGE" "PNG" ("NAME" "icon.png") "<icon.png>" NIL "BASE64" 1986 NIL ("ATTACHMENT" ("FILENAME" "icon.png")) NIL) "RELATED" ("BOUNDARY" "001a114697ae45d5f40565298051") NIL NIL)("MESSAGE" "DELIVERY-STATUS" NIL NIL NIL "7BIT" 609 NIL NIL NIL)("MESSAGE" "RFC822" NIL NIL NIL "7BIT" 3292 ("Wed, 14 Feb 2018 11:23:22 +0100" "=?UTF-8?Q?[User_Notification]:_Milan_Raul=C3=ADm?=" (("=?UTF-8?Q?Milan_Raul=C3=ADm?=" NIL "zakaznik" "spinoco.com")) (("=?UTF-8?Q?Milan_Raul=C3=ADm?=" NIL "zakaznik" "spinoco.com")) ((NIL NIL "zakaznik" "spinoco.com")) ((NIL NIL "kayakoadmin" "spinoco.com")) NIL NIL NIL "<1518603802.5a840e1a1e5cc@zakaznik.spinoco.com>") ("TEXT" "HTML" ("CHARSET" "utf-8") NIL NIL "QUOTED-PRINTABLE" 789 16 NIL NIL NIL) 66 NIL NIL NIL) "REPORT" ("BOUNDARY" "001a114697ae45cdb30565298050" "REPORT-TYPE" "delivery-status") NIL NIL))""".getBytes
    )) ?= Attempt.successful(
      MultiBodyPart(
        Vector(
          MultiBodyPart(
            parts = Vector(
              MultiBodyPart(
                parts = Vector(
                  SingleBodyPart(
                    BodyTypeText("PLAIN", BodyFields(Vector(("CHARSET", "UTF-8")), None, None, "7BIT", 483), 10)
                    , Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
                  )
                  , SingleBodyPart(
                    BodyTypeText("HTML", BodyFields(Vector(("CHARSET", "UTF-8")), None, None, "7BIT", 1745), 35),
                    Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
                  )
                )
                , "ALTERNATIVE"
                , Some(MultiBodyExtension(Vector(("BOUNDARY", "001a114697ae45d6000565298052")), None, Some(List()), None, Vector()))
              )
              , SingleBodyPart(
                BodyTypeBasic(BasicMedia("IMAGE", "PNG"), BodyFields(Vector(("NAME", "icon.png")), Some("<icon.png>"), None, "BASE64", 1986))
                , Some(SingleBodyExtension(None, Some(("ATTACHMENT", Vector(("FILENAME", "icon.png")))), Some(List()), None, Vector()))
              )
            )
            , "RELATED"
            , Some(MultiBodyExtension(Vector(("BOUNDARY", "001a114697ae45d5f40565298051")), None, Some(List()), None, Vector()))
          )
          , SingleBodyPart(
            BodyTypeBasic(BasicMedia("MESSAGE", "DELIVERY-STATUS"), BodyFields(Vector(), None, None, "7BIT", 609))
            , Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
          )
          , SingleBodyPart(
            BodyTypeMessage(
              BodyFields(Vector(), None, None, "7BIT", 3292)
              , Envelope(
                LocalDate.of(2018, 2, 14)
                , Some("=?UTF-8?Q?[User_Notification]:_Milan_Raul=C3=ADm?=")
                , Vector(EmailAddress("zakaznik", "spinoco.com", Some("=?UTF-8?Q?Milan_Raul=C3=ADm?=")))
                , Vector(EmailAddress("zakaznik", "spinoco.com", Some("=?UTF-8?Q?Milan_Raul=C3=ADm?=")))
                , Vector(EmailAddress("zakaznik", "spinoco.com", None))
                , Vector(EmailAddress("kayakoadmin", "spinoco.com", None))
                , Vector(), Vector(), None, Some("<1518603802.5a840e1a1e5cc@zakaznik.spinoco.com>")
              )
              , SingleBodyPart(
                BodyTypeText("HTML", BodyFields(Vector(("CHARSET", "utf-8")), None, None, "QUOTED-PRINTABLE", 789), 16)
                , Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
              ), 66
            )
            , Some(SingleBodyExtension(None, None, Some(List()), None, Vector()))
          )
        )
        , "REPORT"
        , Some(
          MultiBodyExtension(
            Vector(("BOUNDARY", "001a114697ae45cdb30565298050"), ("REPORT-TYPE", "delivery-status"))
            , None
            , Some(List())
            , None
            , Vector()
          )
        )
      )
    )

  }

}
