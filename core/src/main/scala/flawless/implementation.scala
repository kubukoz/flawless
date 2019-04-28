package flawless

import cats.Show
import cats.kernel.Eq
import cats.implicits._
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.Functor

case class RunStats(
  suite: RunStats.Stat,
  test: RunStats.Stat,
  assertion: RunStats.Stat
)

object RunStats {
  case class Stat(total: Int, succesful: Int, failed: Int)

  implicit val eq: Eq[RunStats]     = Eq.fromUniversalEquals
  implicit val show: Show[RunStats] = Show.fromToString
}

case class Location(file: String, line: Int)

object Location {
  implicit val show: Show[Location] = location => show"${location.file}:${location.line}"
}

case class AssertionFailure(text: String, location: Location)

case class Assertions(value: NonEmptyList[Assertion])

object Assertions {
  implicit val semigroup: Semigroup[Assertions] = (a, b) =>
    Assertions(a.value |+| b.value)
}

sealed trait Assertion extends Product with Serializable {
  def isSuccessful: Boolean = fold(true, _ => false)
  def isFailed: Boolean     = !isSuccessful

  def fold[A](successful: => A, failed: AssertionFailure => A): A = this match {
    case Assertion.Successful       => successful
    case Assertion.Failed(failures) => failed(failures)
  }
}

object Assertion {
  case object Successful                       extends Assertion
  case class Failed(failure: AssertionFailure) extends Assertion
}

case class TestResult(name: String, assertions: Assertions)

case class SuiteResult(results: NonEmptyList[TestResult]) extends AnyVal

object SuiteResult {
  implicit val semigroup: Semigroup[SuiteResult] = (a, b) =>
    SuiteResult(a.results |+| b.results)
}

trait Suite { self =>
  def runSuite: IOTest[SuiteResult]
}

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
