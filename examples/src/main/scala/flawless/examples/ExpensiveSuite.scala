package flawless.examples

import cats.data.NonEmptyList
import flawless.dsl._
import flawless.SuiteClass
import flawless.data.Suite
import cats.implicits._

object ExpensiveSuite extends SuiteClass[Nothing] {

  private def fib(n: Long): Long = {
    def go(a: Long, b: Long, n: Long): Long =
      if (n > 0)
        go(b, a + b, n - 1)
      else b

    go(1, 1, n - 2)
  }

  val runSuite: Suite[Nothing] = suite("ExpensiveSuite") {
    tests(
      lazyTest("fib(1-8)") {
        NonEmptyList
          .of(
            1L -> 1L,
            2L -> 1L,
            3L -> 2L,
            4L -> 3L,
            5L -> 5L,
            6L -> 8L
          )
          .reduceMap { case (x, y) =>
            ensureEqual(fib(x), y)
          }
      },
      lazyTest("fib(Int.MaxValue)") {
        ensureEqual(fib(Int.MaxValue.toLong), 6798988702295324957L)
      }
    )
  }

}
