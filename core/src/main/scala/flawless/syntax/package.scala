package flawless

import cats.{Eval, Functor, Id, Show}
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO
import cats.kernel.Semigroup
import cats.kernel.Eq
import flawless.stats.Location

package object syntax {

  def test[F[_]: Functor](name: String)(
    ftest: F[Assertions]
  ): F[SuiteResult] = {
    ftest.map { result =>
      SuiteResult(NonEmptyList.one(TestResult(name, result)))
    }
  }

  def pureTest(name: String): Assertions => IO[SuiteResult] =
    a => IO.pure(test[Id](name)(a))

  def lazyTest(name: String)(assertions: => Assertions): IO[SuiteResult] =
    IO.eval(test[Eval](name)(Eval.later(assertions)))

  /*
   * If you like to write each test in its own line, this is a handy helper that'll make it possible.
   * Instead of combining tests with the semigroup, pass them to this function
   * as you would to e.g. the List(...) constructor.
   */
  def tests[F[_]](
    first: F[SuiteResult],
    others: F[SuiteResult]*
  )(implicit S: Semigroup[F[SuiteResult]]): F[SuiteResult] =
    NonEmptyList(first, others.toList).reduce

  implicit class ShouldBeSyntax[A](private val actual: A) extends AnyVal {

    def shouldBe(expected: A)(
      implicit eq: Eq[A],
      show: Show[A],
      file: sourcecode.File,
      line: sourcecode.Line
    ): Assertions = {
      val assertion =
        if (eq.eqv(actual, expected))
          Assertion.Successful
        else
          Assertion.Failed(
            AssertionFailure(
              show"""Reason: $actual (actual) wasn't equal to $expected (expected)""",
              Location(file.value, line.value)
            )
          )

      Assertions(NonEmptyList.one(assertion))
    }
  }
}
