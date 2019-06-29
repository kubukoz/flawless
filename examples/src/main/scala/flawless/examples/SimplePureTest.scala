package flawless.examples

import cats.implicits._
import flawless.syntax._
import flawless.{Suite, SuiteResult, Tests}

object SimplePureTest extends Suite {

  val runSuite: Tests[SuiteResult] = {
    pureTest("unit is unit") { () shouldBe (()) }
  }
}
