package spinoco.protocol.ldap

import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}
import scodec.Attempt.{Failure, Successful}
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import shapeless.tag
import spinoco.protocol.ldap.elements.Filter
import spinoco.protocol.ldap.elements._

object LDAPMessageSpec extends Properties("LDAPMessage"){

  def verify[A](verify: A, shouldBe: BitVector)(C: Codec[A]): Prop = {
    (C.encode(verify) match {
      case Failure(err) =>
        Prop.falsified :| err.messageWithContext :| "Encode fail"
      case Successful(a) =>
        println("Should have gotten: " + shouldBe.toHex)
        println("Should buto gotten: " + a.toHex)
        a ?= shouldBe
    }) && (C.decode(shouldBe) match {
      case Failure(err) => Prop.falsified :| err.messageWithContext :| "Decode fail"
      case Successful(a) => a.value ?= verify
    })
  }

  property("bind.admin") = protect {
    verify(LDAPMessage(
      1
      , BindRequest(
        3
        , LDAPDN.decode("dc=admin").require
        , BindRequest.Simple(ByteVector.encodeUtf8("admin").right.get)
      )
      , None
    ), BitVector.fromValidHex("30190201016014020103040864633d61646d696e800561646d696e"))(LDAPMessage.codec)
  }

  property("search.root") = protect {
    val data = BitVector.fromValidHex("3038020102633304000a01000a0103020100020100010100870b6f626a656374436c61737330130411737562736368656d61537562656e747279")
    verify(LDAPMessage(
      2
      , SearchRequest(
        baseObject = LDAPDN.decode("").require
        , scope = SearchRequest.SearchScope.baseObject
        , deferAliases = SearchRequest.Aliases.derefAlways
        , sizeLimit = 0
        , timeLimit = 0
        , typesOnly = false
        , filter = Filter.Present(AttributeDescription.TextDescriptor("objectClass"))
        , attributes = Vector(AttributeSelector.Description(AttributeDescription.TextDescriptor("subschemaSubentry")))
      )
      , None
    ), data)(LDAPMessage.codec)
  }

  property("search.with-and-filter") = protect {

    val data = BitVector.fromValidHex("3071020102636c041164633d7965616c696e6b2c64633d636f6d0a01020a0100020132020100010100a00f8702636ea4090402736e300380014a30370402636e0402736e040b646973706c61794e616d6504066d6f62696c65040f74656c6570686f6e654e756d6265720407697050686f6e65")
    verify(LDAPMessage(
      2
      , SearchRequest(
        baseObject = LDAPDN.decode("dc=yealink,dc=com").require
        , scope = SearchRequest.SearchScope.wholeSubtree
        , deferAliases = SearchRequest.Aliases.neverDerefAliases
        , sizeLimit = 50
        , timeLimit = 0
        , typesOnly = false
        , filter = Filter.And(Set(Filter.Present(AttributeDescription.Recognised(AttributeDescription.AttributeType.commonName)), Filter.SubstringFilter(AttributeDescription.TextDescriptor("sn"), SubStrings(Some(SubStrings.Initial(ByteVector.encodeUtf8("J").right.get)), Vector.empty, None))))
        , attributes = Vector(
          AttributeSelector.Description(AttributeDescription.Recognised(AttributeDescription.AttributeType.commonName))
          , AttributeSelector.Description(AttributeDescription.TextDescriptor("sn"))
          , AttributeSelector.Description(AttributeDescription.TextDescriptor("displayName"))
          , AttributeSelector.Description(AttributeDescription.TextDescriptor("mobile"))
          , AttributeSelector.Description(AttributeDescription.TextDescriptor("telephoneNumber"))
          , AttributeSelector.Description(AttributeDescription.TextDescriptor("ipPhone"))
        )
      )
      , None
    ), data)(LDAPMessage.codec)
  }

  property("abandon") = protect {
    val data = BitVector.fromValidHex("3006020103500102")
    verify(LDAPMessage(
      3
      , AbandonRequest(tag[LDAPMessage](2))
      , None
    ), data)(LDAPMessage.codec)
  }

  property("unbind") = protect {
    val data = BitVector.fromValidHex("30050201044200")
    verify(LDAPMessage(
      4
      , UnbindRequest
      , None
    ), data)(LDAPMessage.codec)
  }

}
