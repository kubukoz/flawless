package flawless.examples

import cats.implicits._
import flawless.syntax._
import flawless.Suite
import flawless.SuiteResult
import flawless.Tests

object SimplePureTest extends Suite {

  val runSuite: Tests[SuiteResult] = {
    pureTest("simple things") {
      ((2 + 2) shouldBe 3) |+| (2 shouldBe 3)
    }
  }
}
