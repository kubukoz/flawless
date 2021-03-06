package flawless.examples

import cats.effect.IO

import scala.util.Random
import cats.implicits._
import flawless.dsl._
import flawless.SuiteClass
import flawless.data.Suite

object FlakySuite extends SuiteClass[IO] {
  private val flaky = IO(Random.nextInt(10000)).map(_ =!= 0)

  val runSuite: Suite[IO] = suite("FlakySuite") {
    test("random(10000) =!= 0") {
      flaky.map(ensureEqual(_, false))
    }.combineN(2)
  }

}
