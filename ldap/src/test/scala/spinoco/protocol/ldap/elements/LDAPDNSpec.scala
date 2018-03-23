package spinoco.protocol.ldap.elements

import org.scalacheck.Prop._
import org.scalacheck.Properties
import scodec.Attempt

object LDAPDNSpec extends Properties("LDAPDN"){

  property("encode.decode") = protect {

    LDAPDN.encode(
      LDAPDN.decode("dc=yealink,dc=com").require
    ) ?= Attempt.successful("dc=yealink,dc=com")

  }

  property("encode.decode.escaped.slash") = protect {

    LDAPDN.encode(
      LDAPDN.decode("dc=\\#\\\"\\+\\,\\;\\00\\<\\=\\>\\\\\\ +uid=aaaa,dc=com").require
    ) ?= Attempt.successful("dc=\\23\\\"\\+\\,\\;\\00\\<\\=\\>\\\\\\20+uid=aaaa,dc=com")

  }

}
