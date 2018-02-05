package spinoco.protocol.http

import org.scalacheck.Properties
import org.scalacheck.Prop._

object SchemeSpec extends Properties("Schema"){

  property("parse.valid") = protect{
    Scheme.parseScheme("http-1+3.two") ?= Some(Scheme("http-1+3.two"))
  }

  property("parse.valid.upper-case") = protect{
    Scheme.parseScheme("http-1+3.TWO") ?= Some(Scheme("http-1+3.two"))
  }

  property("parse.invalid.starts-digit") = protect{
    Scheme.parseScheme("1http-1+3.two") ?= None
  }

  property("parse.invalid.non-valid-char") = protect{
    Scheme.parseScheme("http-1:3.two") ?= None
  }

}
