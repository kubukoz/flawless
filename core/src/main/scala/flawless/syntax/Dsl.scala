package flawless.syntax

import flawless.Assertions
import flawless.SuiteResult

import cats.Functor
import cats.implicits._
import cats.data.NonEmptyList
import flawless.TestResult
import cats.kernel.Semigroup
import cats.kernel.Eq
import cats.Show
import flawless.Assertion
import flawless.AssertionFailure
import flawless.stats.Location

class Dsl[F[_]: Functor] {

  def test(name: String)(
    ftest: F[Assertions]
  ): F[SuiteResult] = {
    ftest.map { result =>
      SuiteResult(NonEmptyList.one(TestResult(name, result)))
    }
  }

  /*
   * If you like to write each test in its own line, this is a handy helper that'll make it possible.
   * Instead of combining tests with the semigroup, pass them to this function
   * as you would to e.g. the List(...) constructor.
   */
  def tests(
    first: F[SuiteResult],
    others: F[SuiteResult]*
  )(implicit S: Semigroup[F[SuiteResult]]): F[SuiteResult] =
    NonEmptyList(first, others.toList).reduce

  //anyval maybe?
  implicit class ShouldBeSyntax[A](actual: A) {

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
