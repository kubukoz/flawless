package flawless.examples

import cats.implicits._
import flawless.data.neu._
import flawless.data.neu.dsl._
import flawless.data.neu.predicates.all._

object SimplePureTest extends SuiteClass[Nothing] {

  val runSuite: Suite[Nothing] = suite("SimplePureTest") {
    pureTest("simple things") {
      ensureEqual(2 + 2, 3) |+| ensureEqual(2, 3)
    }
  }
}
