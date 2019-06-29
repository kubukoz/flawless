package flawless.examples

import cats.data.NonEmptyList
import flawless.Suite
import flawless.SuiteResult
import flawless.Tests
import cats.implicits._

object ExpensiveSuite extends Suite {
  import flawless.syntax._

  private def fib(n: Long): Long = {
    def go(a: Long, b: Long, n: Long): Long =
      if (n > 0)
        go(b, a + b, n - 1)
      else b

    go(1, 1, n - 2)
  }

  val runSuite: Tests[SuiteResult] = {
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
          .reduceMap {
            case (x, y) => fib(x) shouldBe y
          }
      },
      lazyTest("fib(Int.MaxValue)") {
        fib(Int.MaxValue.toLong) shouldBe 6798988702295324957L
      }
    )
  }
}
