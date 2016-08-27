package spinoco.protocol.common

import org.scalacheck.Gen

/**
  * Created by pach on 24/07/16.
  */
object generators {

  val ipString:Gen[String] = {
    for {
      b1 <- Gen.choose(0,255)
      b2 <- Gen.choose(0,255)
      b3 <- Gen.choose(0,255)
      b4 <- Gen.choose(0,255)
    } yield s"$b1.$b2.$b3.$b4"
  }


}
