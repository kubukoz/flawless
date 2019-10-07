package flawless.examples

import cats.implicits._
import flawless.SuiteClass
import flawless.dsl._
import flawless.data.Suite

object SimplePureTest extends SuiteClass[Nothing] {

  val runSuite: Suite[Nothing] = suite("SimplePureTest") {
    pureTest("simple things") {
      ensureEqual(2 + 2, 3) |+| ensureEqual(2, 3)
    }
  }
}
