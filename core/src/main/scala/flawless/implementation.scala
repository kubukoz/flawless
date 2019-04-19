package flawless

import cats.Show
import cats.kernel.Eq
import cats.effect.IO
import cats.implicits._
import cats.data.NonEmptyList
import cats.data.Kleisli
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

case class TestRun(
  only: List[String],
  except: List[String]
)

case class Location(file: String, line: Int)

object Location {
  implicit val show: Show[Location] = location => show"${location.file}:${location.line}"
}
case class AssertionFailure(text: String, location: Location)

case class Output(outcomes: NonEmptyList[Outcome])

object Output {
  implicit val semigroup: Semigroup[Output] = (a, b) => Output(a.outcomes |+| b.outcomes)
}

//glorified Validated
sealed trait Outcome extends Product with Serializable {
  def isSuccessful: Boolean = fold(true, _ => false)
  def isFailed: Boolean     = !isSuccessful

  def fold[A](successful: => A, failed: AssertionFailure => A): A = this match {
    case Outcome.Successful       => successful
    case Outcome.Failed(failures) => failed(failures)
  }
}

object Outcome {
  case object Successful                       extends Outcome
  case class Failed(failure: AssertionFailure) extends Outcome
}

case class TestResult(name: String, output: Output)

case class SuiteResult(results: NonEmptyList[TestResult])

object SuiteResult {
  implicit val semigroup: Semigroup[SuiteResult] = (a, b) =>
    SuiteResult(a.results |+| b.results)
}

trait Suite { self =>
  def runSuite: IOTest[SuiteResult]
}

trait PureSuite extends Suite {
  def runSuitePure: PureTest[SuiteResult]

  final override val runSuite: IOTest[SuiteResult] = Kleisli { config =>
    IO(runSuitePure.run(config))
  }
}

class Dsl[F[_]: Functor] {

  def test(name: String)(
    ftest: F[Output]
  ): Kleisli[F, TestRun, SuiteResult] = Kleisli.liftF {
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
    first: Kleisli[F, TestRun, SuiteResult],
    others: Kleisli[F, TestRun, SuiteResult]*
  )(implicit S: Semigroup[F[SuiteResult]]): Kleisli[F, TestRun, SuiteResult] =
    NonEmptyList(first, others.toList).reduce

  //anyval maybe?
  implicit class ShouldBeSyntax[A](actual: A) {

    def shouldBe(expected: A)(
      implicit eq: Eq[A],
      show: Show[A],
      file: sourcecode.File,
      line: sourcecode.Line
    ): Output = {
      val outcome =
        if (eq.eqv(actual, expected))
          Outcome.Successful
        else
          Outcome.Failed(
            AssertionFailure(
              show"""$actual (actual) wasn't equal to $expected (expected)""",
              Location(file.value, line.value)
            )
          )

      Output(NonEmptyList.one(outcome))
    }
  }
}
