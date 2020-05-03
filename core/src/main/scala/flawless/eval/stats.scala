package flawless.eval

import cats.Eq
import cats.Show
import cats.implicits._
import cats.data.NonEmptyList
import cats.data.NonEmptyChain
import cats.Id
import flawless.data.Suite
import flawless.data.Test
import flawless.data.Assertion
import flawless.data.TestRun
import monocle.Getter
import monocle.Fold
import cats.kernel.Monoid
import flawless.data.Assertion.Result.Successful
import flawless.data.Assertion.Result.Pending
import flawless.data.Assertion.Result.Failed

final case class RunStats(suite: RunStats.Stat, test: RunStats.Stat, assertion: RunStats.Stat)

object RunStats {

  def fromSuites(suite: Suite[Id]): RunStats = {
    val stats = getStat(suite)

    val suiteStat = stats.of(optics.suiteToOnes, optics.oneToAssertions)
    val testStat = stats.of(optics.suiteToTests, optics.testToAssertionResults)
    val assertionStat = stats.of(optics.suiteToAssertions, Fold.id)

    RunStats(
      suite = suiteStat,
      test = testStat,
      assertion = assertionStat
    )
  }

  implicit val monoid: Monoid[RunStats] =
    (Monoid[Stat], Monoid[Stat], Monoid[Stat]).imapN(RunStats.apply)(s => (s.suite, s.test, s.assertion))

  //Getters/folds for suite/test/assertion results
  private object optics {

    private val suiteOnes: Getter[Suite[Id], NonEmptyList[Suite.algebra.One[Id]]] =
      Suite.flatten(_)

    private val oneTests: Getter[Suite.algebra.One[Id], NonEmptyList[Test[Id]]] =
      _.tests

    private val testRunToAssertions: Getter[TestRun[Id], Assertion] =
      _.assertions[Id]

    private val testAssertions: Getter[Test[Id], Assertion] =
      Getter((_: Test[Id]).result).composeGetter(testRunToAssertions)

    private val assertionResults: Getter[Assertion, NonEmptyChain[Assertion.Result]] = Getter(_.results)

    val suiteToOnes: Fold[Suite[Id], Suite.algebra.One[Id]] =
      suiteOnes.composeFold(Fold.fromFoldable)

    val oneToTests: Fold[Suite.algebra.One[Id], Test[Id]] =
      oneTests.composeFold(Fold.fromFoldable)

    val suiteToTests: Fold[Suite[Id], Test[Id]] =
      suiteToOnes.composeFold(oneToTests)

    val testToAssertionResults: Fold[Test[Id], Assertion.Result] =
      testAssertions.composeGetter(assertionResults).composeFold(Fold.fromFoldable)

    val oneToAssertions: Fold[Suite.algebra.One[Id], Assertion.Result] =
      oneToTests composeFold testToAssertionResults

    val suiteToAssertions: Fold[Suite[Id], Assertion.Result] =
      suiteToOnes composeFold oneToAssertions
  }

  private def getStat(suite: Suite[Id]) = new GetStatsPartiallyApplied(suite)

  final class GetStatsPartiallyApplied private[RunStats] (suite: Suite[Id]) {

    /** Get the stats for the selected metric (as defined by the `shallow` fold) of all the suites in `suite`.
      * `deep` defines how to go from the selected metric to the assertion results.
      */
    def of[Selected](shallow: Fold[Suite[Id], Selected], deep: Fold[Selected, Assertion.Result]): RunStats.Stat =
      shallow.foldMap(deep.foldMap(Stat.fromResult)(_).summarize)(suite)
  }

  final case class Stat(successful: Int, pending: Int, failed: Int) {
    def total: Int = successful + pending + failed

    // Summarizes this stat for the bigger picture - e.g. if it's for assertions and (successful = 1, failed = 1) - it's a failed test.
    // Rounds up to the "worst" non-negative result. Always results in a stat with total = 1.
    def summarize: Stat =
      NonEmptyList
        .of(
          failed -> Stat.failure,
          pending -> Stat.pend
        )
        .collectFirst { case (n, stat) if n > 0 => stat }
        .getOrElse(Stat.success)

  }

  object Stat {
    val success = Stat(1, 0, 0)
    val pend = Stat(0, 1, 0)
    val failure = Stat(0, 0, 1)

    val fromResult: Assertion.Result => Stat = {
      case Successful => success
      case Pending    => pend
      case Failed(_)  => failure
    }

    implicit val monoid: Monoid[Stat] =
      (Monoid[Int], Monoid[Int], Monoid[Int]).imapN(Stat.apply)(s => (s.successful, s.pending, s.failed))
  }

  implicit val eq: Eq[RunStats] = Eq.fromUniversalEquals
  implicit val show: Show[RunStats] = Show.fromToString
}

//maybe this one can go to meta
final case class Location(file: String, line: Int)

object Location {
  implicit val show: Show[Location] = location => show"${location.file}:${location.line}"
}
