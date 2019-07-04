package flawless

import cats.Eval
import cats.Show
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO
import cats.kernel.Eq
import cats.kernel.Semigroup
import flawless.stats.Location

package object syntax {

  def test(name: String)(ftest: IO[Assertions]): Tests[SuiteResult] =
    Tests.liftIO(ftest.map(toResult(name, _)))

  def pureTest(name: String)(assertions: Assertions): Tests[SuiteResult] =
    Tests.pure(toResult(name, assertions))

  def lazyTest(name: String)(assertions: => Assertions): Tests[SuiteResult] =
    test(name)(IO.eval(Eval.later(assertions)))

  private def toResult(name: String, result: Assertions): SuiteResult =
    SuiteResult(NonEmptyList.one(TestResult(name, result)))

  /*
   * If you like to write each test in its own line, this is a handy helper that'll make it possible.
   * Instead of combining tests with the semigroup, pass them to this function
   * as you would to e.g. the List(...) constructor.
   */
  def tests[A: Semigroup](first: Tests[A], others: Tests[A]*): Tests[A] =
    NonEmptyList(first, others.toList).reduce

  implicit class ShouldBeSyntax[A](private val actual: A) extends AnyVal {

    def shouldBe(expected: A)(implicit eq: Eq[A], show: Show[A], file: sourcecode.File, line: sourcecode.Line): Assertions = {
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

      Assertions(assertion)
    }
  }

  /**
    * Import this if you don't give a damn.
    * Default instances of type classes that allow you to write some tests quicker.
    */
  object idgaf {
    implicit def anyEq[T]: Eq[T] = Eq.fromUniversalEquals
    implicit def anyShow[T]: Show[T] = Show.fromToString
  }
}
