package spinoco.protocol.common

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.Matchers
import org.scalatest.concurrent.{Eventually, TimeLimitedTests}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.time.SpanSugar._

/**
  * Created by pach on 23/07/16.
  */
class ProtocolSpec extends AnyFreeSpec
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with TimeLimitedTests
  with Eventually {


  val timeLimit = 900.seconds

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(timeout = timeLimit)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50, workers = 1)

}
