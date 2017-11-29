package spinoco.protocol.mail.imap.codec

import org.scalacheck.Properties
import org.scalacheck.Prop._
import scodec.Attempt
import scodec.bits.BitVector
import spinoco.protocol.mail.imap.BodyStructure._

object IMAPBodyPartCodecSpec extends Properties("IMAPBodyPartCodec") {

  property("simple-body-structure") = protect {

  IMAPBodyPartCodec.bodyStructure.decodeValue(BitVector.view(
    """(BODYSTRUCTURE (("TEXT" "HTML" ("CHARSET" "UTF-8") NIL NIL "QUOTED-PRINTABLE" 22621 598 NIL ("INLINE" NIL) NIL)("IMAGE" "PNG" ("NAME" "page.word-icon.png") "<page.word-icon>" NIL "BASE64" 328 NIL ("INLINE" ("FILENAME" "page.word-icon.png")) NIL)("IMAGE" "PNG" ("NAME" "footer-desktop-logo.png") "<footer-desktop-logo>" NIL "BASE64" 1182 NIL ("INLINE" ("FILENAME" "footer-desktop-logo.png")) NIL)("IMAGE" "PNG" ("NAME" "footer-mobile-logo.png") "<footer-mobile-logo>" NIL "BASE64" 524 NIL ("INLINE" ("FILENAME" "footer-mobile-logo.png")) NIL) "RELATED" ("BOUNDARY" "----=_Part_11768_185568052.1510095660090") NIL NIL))""".getBytes
  )) ?= Attempt.successful(
    MultiBodyPart(
      parts = Vector(
        SingleBodyPart(
          tpe = BodyTypeText(
            subType = "HTML"
            , fields = BodyFields(
              params = Vector(("CHARSET", "UTF-8"))
              , id = None
              , desc = None
              , encoding = "QUOTED-PRINTABLE"
              , size = 22621
            )
            , lines = 598
          )
          , ext = Some(SingleBodyExtension(
            md5 = None
            , dsp = Some(("INLINE", Vector.empty))
            , lang = Some(List())
            , loc = None
            , extensions = Vector()
          ))
        )
        , SingleBodyPart(
          tpe = BodyTypeBasic(
            media = BasicMedia("IMAGE","PNG")
            , fields = BodyFields(
              params = Vector(("NAME", "page.word-icon.png"))
              , id = Some("<page.word-icon>")
              , desc = None
              , encoding = "BASE64"
              , size = 328
            )
          )
          , ext = Some(SingleBodyExtension(
            md5 = None
            , dsp = Some(("INLINE", Vector(("FILENAME", "page.word-icon.png"))))
            , lang = Some(List())
            , loc = None
            , extensions = Vector()
          ))
        )
        , SingleBodyPart(
          tpe = BodyTypeBasic(
            media = BasicMedia("IMAGE","PNG")
            , fields = BodyFields(
              params = Vector(("NAME", "footer-desktop-logo.png"))
              , id = Some("<footer-desktop-logo>")
              , desc = None
              , encoding = "BASE64"
              , size = 1182
            )
          )
          , ext = Some(SingleBodyExtension(
            md5 = None
            , dsp = Some(("INLINE", Vector(("FILENAME", "footer-desktop-logo.png"))))
            , lang = Some(List())
            , loc = None
            , extensions = Vector()
          ))
        )
        , SingleBodyPart(
          tpe = BodyTypeBasic(
            media = BasicMedia("IMAGE","PNG")
            , fields = BodyFields(
              params = Vector(("NAME", "footer-mobile-logo.png"))
              , id = Some("<footer-mobile-logo>")
              , desc = None
              , encoding = "BASE64"
              , size = 524
            )
          )
          , ext = Some(SingleBodyExtension(
            md5 = None
            , dsp = Some(("INLINE", Vector(("FILENAME", "footer-mobile-logo.png"))))
            , lang = Some(List())
            , loc = None
            , extensions = Vector()
          ))
        )
      )
      , mediaSubType = "RELATED"
      , ext = Some(MultiBodyExtension(
        params = Vector(("BOUNDARY", "----=_Part_11768_185568052.1510095660090"))
        , dsp = None
        , lang = Some(Nil)
        , loc = None
        , extensions = Vector.empty
      ))
    )
  )

  }


  property("simple-body-structure.lowercase") = protect {
    IMAPBodyPartCodec.bodyStructure.decodeValue(BitVector.view(
      """(BODYSTRUCTURE (("text" "html" ("charset" "utf-8") NIL NIL "quoted-printable" 22621 598 nil ("inline" nil) nil)("image" "png" ("name" "page.word-icon.png") "<page.word-icon>" nil "base64" 328 NIL ("inline" ("filename" "page.word-icon.png")) nil)("image" "png" ("name" "footer-desktop-logo.png") "<footer-desktop-logo>" nil "base64" 1182 nil ("inline" ("filename" "footer-desktop-logo.png")) nil)("image" "png" ("name" "footer-mobile-logo.png") "<footer-mobile-logo>" nil "base64" 524 nil ("inline" ("filename" "footer-mobile-logo.png")) nil) "related" ("boundary" "----=_Part_11768_185568052.1510095660090") nil nil))""".getBytes
    )) ?= Attempt.successful(
      MultiBodyPart(
        parts = Vector(
          SingleBodyPart(
            tpe = BodyTypeText(
              subType = "html"
              , fields = BodyFields(
                params = Vector(("charset", "utf-8"))
                , id = None
                , desc = None
                , encoding = "quoted-printable"
                , size = 22621
              )
              , lines = 598
            )
            , ext = Some(SingleBodyExtension(
              md5 = None
              , dsp = Some(("inline", Vector.empty))
              , lang = Some(List())
              , loc = None
              , extensions = Vector()
            ))
          )
          , SingleBodyPart(
            tpe = BodyTypeBasic(
              media = BasicMedia("image","png")
              , fields = BodyFields(
                params = Vector(("name", "page.word-icon.png"))
                , id = Some("<page.word-icon>")
                , desc = None
                , encoding = "base64"
                , size = 328
              )
            )
            , ext = Some(SingleBodyExtension(
              md5 = None
              , dsp = Some(("inline", Vector(("filename", "page.word-icon.png"))))
              , lang = Some(List())
              , loc = None
              , extensions = Vector()
            ))
          )
          , SingleBodyPart(
            tpe = BodyTypeBasic(
              media = BasicMedia("image","png")
              , fields = BodyFields(
                params = Vector(("name", "footer-desktop-logo.png"))
                , id = Some("<footer-desktop-logo>")
                , desc = None
                , encoding = "base64"
                , size = 1182
              )
            )
            , ext = Some(SingleBodyExtension(
              md5 = None
              , dsp = Some(("inline", Vector(("filename", "footer-desktop-logo.png"))))
              , lang = Some(List())
              , loc = None
              , extensions = Vector()
            ))
          )
          , SingleBodyPart(
            tpe = BodyTypeBasic(
              media = BasicMedia("image","png")
              , fields = BodyFields(
                params = Vector(("name", "footer-mobile-logo.png"))
                , id = Some("<footer-mobile-logo>")
                , desc = None
                , encoding = "base64"
                , size = 524
              )
            )
            , ext = Some(SingleBodyExtension(
              md5 = None
              , dsp = Some(("inline", Vector(("filename", "footer-mobile-logo.png"))))
              , lang = Some(List())
              , loc = None
              , extensions = Vector()
            ))
          )
        )
        , mediaSubType = "related"
        , ext = Some(MultiBodyExtension(
          params = Vector(("boundary", "----=_Part_11768_185568052.1510095660090"))
          , dsp = None
          , lang = Some(Nil)
          , loc = None
          , extensions = Vector.empty
        ))
      )
    )

  }

}
