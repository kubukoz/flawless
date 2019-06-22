package flawless.examples

import flawless.Suite
import flawless.syntax._
import cats.implicits._
import flawless.SuiteResult
import flawless.Tests

object SimplePureTest extends Suite {

  val runSuite: Tests[SuiteResult] = {
    pureTest("unit is unit") { () shouldBe (()) }
  }
}
