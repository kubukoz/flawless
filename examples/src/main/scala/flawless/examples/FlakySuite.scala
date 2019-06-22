package flawless.examples

import cats.effect.IO
import flawless.Suite
import flawless.SuiteResult

import scala.util.Random
import cats.implicits._
import flawless.Tests

object FlakySuite extends Suite {
  import flawless.syntax._
  private val flaky = IO(Random.nextInt(10000)).map(_ =!= 0)

  val runSuite: Tests[SuiteResult] = test("random(10000) =!= 0") {
    flaky.map(_ shouldBe true)
  }.combineN(2)
}
