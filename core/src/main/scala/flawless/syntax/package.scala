package flawless

import cats.Functor
import cats.implicits._
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.kernel.Eq
import cats.Show
import flawless.stats.Location

package object syntax {

  def test[A, F[_]](name: String)(
    ftest: A
  )(
    implicit structure: Structure[A, F, Assertions],
    functor: Functor[F]
  ): F[SuiteResult] = {
    structure.convert(ftest).map { result =>
      SuiteResult(NonEmptyList.one(TestResult(name, result)))
    }
  }

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
