package flawless.examples

import cats.effect.IO
import flawless.{IOTest, Suite, SuiteResult}

import scala.util.Random
import cats.implicits._

object FlakySuite extends Suite {
  import flawless.syntax.io._
  private val flaky = IO(Random.nextInt(10000)).map(_ =!= 0)

  val runSuite: IOTest[SuiteResult] = test("random(10000) =!= 0") {
    flaky.map(_ shouldBe true)
  }
}
