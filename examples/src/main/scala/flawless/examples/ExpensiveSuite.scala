package flawless.examples

import cats.Eval
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO
import flawless.{IOTest, Suite, SuiteResult}

object ExpensiveSuite extends Suite {
  import flawless.syntax.eval._

  private def fib(n: Long): Long = {
    def go(a: Long, b: Long, n: Long): Long = {
      if (n > 0)
        go(b, a + b, n - 1)
      else b
    }

    go(1, 1, n - 2)
  }

  val runSuite: IOTest[SuiteResult] = IO.eval {
    tests(
      test("fib(1-8)") {
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
            case (x, y) => Eval.later(fib(x)).map(_ shouldBe y)
          }
      },
      test("fib(Int.MaxValue)") {
        Eval.later(fib(Int.MaxValue.toLong)).map(_ shouldBe 6798988702295324957L)
      }
    )
  }
}
