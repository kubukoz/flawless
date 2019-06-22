package flawless.examples

import flawless.Suite
import flawless.syntax._
import cats.implicits._
import cats.effect.IO
import flawless.SuiteResult

object SimplePureTest extends Suite {

  val runSuite: IO[SuiteResult] = {
    pureTest("unit is unit") { () shouldBe (()) }
  }
}
