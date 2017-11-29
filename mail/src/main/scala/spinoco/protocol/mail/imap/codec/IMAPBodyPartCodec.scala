package spinoco.protocol.mail.imap.codec

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scodec._
import scodec.bits.BitVector
import scodec.codecs._
import spinoco.protocol.common.codec._
import spinoco.protocol.common.util.attempt
import spinoco.protocol.mail.EmailAddress
import spinoco.protocol.mail.imap.BodyStructure._
import shapeless.{::, HNil}


object IMAPBodyPartCodec {
  import impl._


  lazy val bodyStructure: Codec[BodyPart] = {
    `(` ~> constantString1CaseInsensitive("BODYSTRUCTURE") ~> SP ~> codec <~ `)`
  }

  lazy val codec: Codec[BodyPart] = {
    `(` ~>
      choice(
        ("multi body"         | multiBodyPart.upcast[BodyPart])
        , ("single body"      | singleBodyPart.upcast[BodyPart])
      ) <~
     `)`
  }

  object impl {

    val bits_SP : BitVector = BitVector.view(" ".getBytes())

    val `(` : Codec[Unit] = "(" | constantString1("(")
    val `)` : Codec[Unit] = ")" | constantString1(")")
    val `{` : Codec[Unit] = "{" | constantString1("{")
    val `}` : Codec[Unit] = "}" | constantString1("}")
    val `}crlf` : Codec[Unit] = "}crlf" | constantString1("}\r\n")
    val SP  : Codec[Unit] = "SP" | constantString1(" ")
    val NIL : Codec[Unit] = "NIL" | constantString1CaseInsensitive("NIL")
    def nilVector[A]: Codec[Vector[A]] = NIL.widen[Vector[A]](_ => Vector.empty, v => if(v.isEmpty) Attempt.successful(()) else Attempt.failure(Err("Empty list required")))
    val DQUOTE  : Codec[Unit] = "DQUOTE" | constantString1("\"")

    lazy val intNumber = takeWhile(intAsString)(b => b >= '0' && b <= '9')

    val quotedString : Codec[String] =
      "quoted-string" | (DQUOTE ~> (takeWhile(ascii)(_ != '"') <~ DQUOTE))

    val literalString  : Codec[String] = {
      val sz: Codec[Int] = `{` ~> ( intNumber <~ `}crlf`)
      "literal-string" | variableSizeBytes(sz, ascii)
    }

    lazy val string: Codec[String] = "string" | choice(quotedString, literalString)
    lazy val nstring: Codec[Option[String]] =
      "nstring" | choice(
        NIL.xmap[None.type](_ => None, _ => ()).upcast[Option[String]]
        , string.xmap[Some[String]](Some(_), _.get).upcast[Option[String]]
      )




    def envelope: Codec[Envelope] = {
      `(`  ~> (
        ("date"         | envDate) ::
        ("subject"      | (SP ~> envSubject)) ::
        ("from"         | (SP ~> envFrom)) ::
        ("sender"       | (SP ~> envSender)) ::
        ("reply-to"     | (SP ~> envReplyTo)) ::
        ("to"           | (SP ~> envTo)) ::
        ("cc"           | (SP ~> envCc)) ::
        ("bcc"          | (SP ~> envBcc)) ::
        ("in-reply-to"  | (SP ~> envInReplyTo)) ::
        ("message-id"   | (SP ~> envMessageId)) <~
      `)`
      ).as[Envelope] <~ `)`
    }


    def envDate: Codec[LocalDate] = {
      val format = DateTimeFormatter.ofPattern("dd-MMM-yyyy")
      val date: Codec[LocalDate] = {
        ascii.narrow(
          s => attempt { LocalDate.parse(s, format) }
          , dt => dt.format(format)
        )
      }

      choice(
        DQUOTE ~> date <~ DQUOTE
        , date
      )
    }

    private def emailAddress: Codec[EmailAddress] = {
      val name: Codec[Option[String]] = nstring
      val adl: Codec[Option[String]] = nstring
      val mailbox: Codec[Option[String]] = nstring
      val host: Codec[Option[String]] = nstring

      (`(` ~> (
        name ::
        adl ::
        mailbox ::
        host
        ) <~ `)`
      ).xmap[EmailAddress] (
        { case n :: _ :: m :: h :: HNil =>
          EmailAddress(
            localPart = m.getOrElse("")
            , domain = h.getOrElse("")
            , display = n
          )
        }
        , e =>
          e.display :: None :: Option(e.localPart).filter(_.nonEmpty) :: Option(e.domain).filter(_.nonEmpty) :: HNil
      )
    }

    private def emailAddressesList: Codec[Vector[EmailAddress]] = {
      choice(
        NIL.widen[Vector[EmailAddress]](_ => Vector.empty, v => if (v.isEmpty) Attempt.successful(()) else Attempt.failure(Err("List must be empty")))
        , `(` ~> minItems(1)(vectorV(emailAddress)) <~ `)`
      )
    }

    def envBcc: Codec[Vector[EmailAddress]] = emailAddressesList
    def envCc:  Codec[Vector[EmailAddress]] = emailAddressesList
    def envFrom: Codec[Vector[EmailAddress]] = emailAddressesList
    def envTo: Codec[Vector[EmailAddress]] = emailAddressesList
    def envReplyTo: Codec[Vector[EmailAddress]] = emailAddressesList
    def envSender: Codec[Vector[EmailAddress]] = emailAddressesList

    def envInReplyTo: Codec[Option[String]] = nstring
    def envMessageId: Codec[Option[String]] = nstring
    def envSubject: Codec[Option[String]] = nstring


    def singleBodyPart : Codec[SingleBodyPart] = {
      (
        choice(
          bodyTypeText.upcast[BodyType]
          , bodyTypeMessage.upcast[BodyType]
          , bodyTypeBasic.upcast[BodyType]
        ) ::
        optional(lookahead2(SP), SP ~> singleBodyExtension)
      ).as
    }


    def  multiBodyPart : Codec[MultiBodyPart] = {
      (
        ("parts"             | minItems(1)(vectorV(lazily(codec)))) ::
        ("media-sub-type"  | (SP ~> mediaSubtype)) ::
        ("extension"       | optional(lookahead2(SP), SP ~> multiBodyExtension))
      ).as
    }


    def bodyTypeText: Codec[BodyTypeText] =  {
      (
        ("media-text"       | mediaText) ::
        ("body-fields"      | (SP ~> bodyFields)) ::
        ("body-lines"       | (SP ~> bodyFldLines))
      ).as
    }

    def bodyTypeMessage: Codec[BodyTypeMessage] = {
      (
        ("media-message"  | mediaMessage) :~>:
        ("body-fields"    | (SP ~> bodyFields)) ::
        ("envelope"       | (SP ~> envelope)) ::
        ("body"           | (SP ~> lazily(codec))) ::
        ("body-lines"     | (SP ~> bodyFldLines))
      ).as

    }

    def bodyTypeBasic: Codec[BodyTypeBasic] = {
      (
        ("media-basic"    | mediaBasic) ::
        ("body-fields"    | (SP ~> bodyFields))
      ).as
    }

    def bodyFields: Codec[BodyFields] = {
      (
        ("fld-params" | bodyFldParam) ::
        ("fld-id" | (SP ~> bodyFldId)) ::
        ("fld-desc" | (SP ~> bodyFldDesc)) ::
        ("fld-encoding" | (SP ~> bodyFldEnc)) ::
        ("fld-octets" | (SP ~> bodyFldOctets))
      ).as
    }

    def bodyFldLines: Codec[Int] = intNumber

    def  bodyFldParam: Codec[Vector[(String, String)]] = {
      choice(
        nilVector
        , `(` ~> vectorVDelimited(string ~ (SP ~> string), SP) <~ `)`
      )
    }


    def bodyFldId: Codec[Option[String]] = nstring

    def bodyFldDesc: Codec[Option[String]] = nstring

    def bodyFldEnc: Codec[String] = string

    def bodyFldOctets: Codec[Int] = intNumber

    def bodyFldMD5:  Codec[Option[String]] = nstring

    def bodyFldDsp:  Codec[Option[(String, Vector[(String, String)])]] = {
      choice(
        NIL.xmap[None.type](_ => None, _ => ()).upcast[Option[(String, Vector[(String, String)])]]
        ,  (`(` ~> (string ~ (SP ~> bodyFldParam)) <~ `)`)
           .xmap[Some[(String, Vector[(String, String)])]](Some(_), _.get)
           .upcast
      )
    }

    def bodyFldLang:  Codec[Vector[String]] = {
      choice(
        `(` ~> minItems(2)(vectorVDelimited(string, SP)) <~ `)`
        , maxItems(1)(nstring.xmap(_.toVector, _.headOption))
      )
    }


    lazy val bodyFldLoc:  Codec[Option[String]] =
      nstring

    def singleBodyExtension: Codec[SingleBodyExtension] = {
      (
        bodyFldMD5 ~
          optional(lookahead2(SP), SP ~> bodyFldDsp ~
            optional(lookahead2(SP), SP ~> bodyFldLang ~
              optional(lookahead2(SP), SP ~> bodyFldLoc ~ vectorVDelimited(bodyExtension, SP) )
            )
        )
      ).xmap(
        { case (md5, next) =>
          SingleBodyExtension(
            md5 = md5
            , dsp = next.flatMap(_._1)
            , lang = next.flatMap(_._2.map(_._1.toList))
            , loc = next.flatMap(_._2.flatMap(_._2.flatMap(_._1)))
            , extensions =  next.flatMap(_._2.flatMap(_._2.map(_._2))).getOrElse(Vector.empty)
          )
        }
        , ext => {
          val a = Option((ext.loc, ext.extensions)).filter(_ => ext.loc.nonEmpty || ext.extensions.nonEmpty)
          val b = Option((ext.lang.map(_.toVector).getOrElse(Vector.empty), a)).filter(_ => ext.lang.exists(_.nonEmpty) || a.nonEmpty)
          val c = Option((ext.dsp, b)).filter(_ => ext.dsp.nonEmpty || b.nonEmpty)
          (ext.md5, c)
        }
      )
    }

    def multiBodyExtension: Codec[MultiBodyExtension] = {
      (
        bodyFldParam ~
        optional(lookahead2(SP), SP ~> bodyFldDsp ~
          optional(lookahead2(SP), SP ~> bodyFldLang ~
            optional(lookahead2(SP), SP ~> bodyFldLoc ~ vectorVDelimited(bodyExtension, SP) )
          )
        )
      ).xmap(
        { case (params, next) =>
          MultiBodyExtension(
            params = params
            , dsp = next.flatMap(_._1)
            , lang = next.flatMap(_._2.map(_._1.toList))
            , loc = next.flatMap(_._2.flatMap(_._2.flatMap(_._1)))
            , extensions =  next.flatMap(_._2.flatMap(_._2.map(_._2))).getOrElse(Vector.empty)
          )
        }
        , ext => {
          val a = Option((ext.loc, ext.extensions)).filter(_ => ext.loc.nonEmpty || ext.extensions.nonEmpty)
          val b = Option((ext.lang.map(_.toVector).getOrElse(Vector.empty), a)).filter(_ => ext.lang.exists(_.nonEmpty) || a.nonEmpty)
          val c = Option((ext.dsp, b)).filter(_ => ext.dsp.nonEmpty || b.nonEmpty)
          (ext.params, c)
        }
      )

    }

    def bodyExtension: Codec[BodyExtension] = {
      val nil: Codec[ListBodyExtension] = {
        NIL.widen[ListBodyExtension](
          _ => ListBodyExtension(Vector.empty)
          , ls => {
            if (ls.ext.isEmpty) Attempt.successful(())
            else Attempt.failure(Err("Must be empty"))
          })
      }

      val stringExt: Codec[StringBodyExtension] =
        string.xmap[StringBodyExtension](
          s => StringBodyExtension(s)
          , _.ext
        )

      val intExt: Codec[IntBodyExtension] =
        intNumber.xmap[IntBodyExtension](
          i => IntBodyExtension(i)
          , _.ext
        )

      val listExt : Codec[ListBodyExtension] = {
        (`(` ~> vectorDelimited(bits_SP, lazily(bodyExtension)) <~ `)`).exmap[ListBodyExtension](
          ext => {
            if (ext.isEmpty) Attempt.failure(Err("Must be nonempty"))
            else Attempt.successful(ListBodyExtension(ext))
          }
          , ls => {
            if (ls.ext.isEmpty) Attempt.failure(Err("Must be nonempty"))
            else Attempt.successful(ls.ext)
          }
        )
      }

      choice(
        nil.upcast
        , stringExt.upcast
        , listExt.upcast
        , intExt.upcast
      )
    }

    def mediaBasic: Codec[BasicMedia] = {
      (string :: (SP ~> string)).as
    }

    def mediaSubtype: Codec[String] = string

    def mediaText: Codec[String] = {
      DQUOTE ~> constantString1CaseInsensitive("TEXT") ~> DQUOTE ~> SP ~> string
    }

    def mediaMessage: Codec[Unit] = {
      DQUOTE ~> constantString1CaseInsensitive("MESSAGE") ~> DQUOTE ~> SP ~>
      DQUOTE ~> constantString1CaseInsensitive("RFC822") ~> DQUOTE
    }

  }

}
