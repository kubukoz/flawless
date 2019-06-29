package flawless.examples

import cats.implicits._
import flawless.syntax._
import flawless.{Suite, SuiteResult, TTest}

object SimplePureTest extends Suite {

  val runSuite: TTest[SuiteResult] = {
    pureTest("unit is unit") { () shouldBe (()) }
  }
}
