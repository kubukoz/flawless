package flawless.stats

import cats.Eq
import cats.Foldable
import cats.Show
import cats.implicits._
import cats.data.NonEmptyList
import cats.Id
import flawless.data.Suite
import flawless.data.Test
import flawless.data.Assertion
import flawless.data.TestRun
import monocle.Getter
import monocle.Fold

final case class RunStats(suite: RunStats.Stat, test: RunStats.Stat, assertion: RunStats.Stat)

object RunStats {

  def fromSuites[F[_]: Foldable](suites: F[Suite[Id]]): RunStats = {
    val stats = getStat(suites)

    val suiteStat = stats.of(Fold.id, optics.suiteToAssertions)
    val testStat = stats.of(optics.suiteToTests, optics.testToAssertionResults)
    val assertionStat = stats.of(optics.suiteToAssertions, Fold.id)

    RunStats(
      suite = suiteStat,
      test = testStat,
      assertion = assertionStat
    )
  }

  //Getters/folds for suite/test/assertion results
  private object optics {

    private val suiteTests: Getter[Suite[Id], NonEmptyList[Test[Id]]] =
      Getter(Suite.flatten.andThen(_.flatMap(_.tests)))

    private val testRunToAssertions: Getter[TestRun[Id], Assertion] =
      Getter(_.assertions[Id])

    private val testAssertions: Getter[Test[Id], Assertion] =
      Getter((_: Test[Id]).result).composeGetter(testRunToAssertions)

    private val assertionResults: Getter[Assertion, NonEmptyList[Assertion.AssertionResult]] = Getter(_.results)

    val suiteToTests: Fold[Suite[Id], Test[Id]] =
      suiteTests.composeFold(Fold.fromFoldable)

    val testToAssertionResults: Fold[Test[Id], Assertion.AssertionResult] =
      testAssertions.composeGetter(assertionResults).composeFold(Fold.fromFoldable)

    val suiteToAssertions: Fold[Suite[Id], Assertion.AssertionResult] =
      optics.suiteToTests composeFold optics.testToAssertionResults
  }

  /**
    * For each element `a` in `fa`, partition all the selected elements found in `a` by the given predicate.
    *
    * @param fa the root container (always the same on the same tree)
    * @param select the way to select all the elements you want to end up in the list
    * @param p the predicate that checks the elements
    * @return (matching, nonmatching)
    */
  private def partitionAll[F[_]: Foldable, Root, Selected](
    fa: F[Root],
    select: Fold[Root, Selected],
    p: Selected => Boolean
  ): (List[Selected], List[Selected]) =
    fa.foldMap(select.getAll(_).partition(p))

  private def getStat[F[_]: Foldable](suites: F[Suite[Id]]) = new GetStatsPartiallyApplied(suites)

  final class GetStatsPartiallyApplied[F[_]: Foldable] private[RunStats] (suites: F[Suite[Id]]) {

    /**
      * Get the stats for the selected metric (as defined by the `select` traversal) of all the suites in `fa`.
      * `traversal` defines how to go from the selected metric to the assertions.
      * */
    def of[Selected](
      select: Fold[Suite[Id], Selected],
      traversal: Fold[Selected, Assertion.AssertionResult]
    ): RunStats.Stat = {
      val (succeeded, failed) =
        partitionAll(suites, select, traversal.all(_.isSuccessful))

      val successCount = succeeded.size
      val failureCount = failed.size

      RunStats.Stat(failureCount + successCount, successCount, failureCount)
    }
  }

  final case class Stat(total: Int, successful: Int, failed: Int)

  implicit val eq: Eq[RunStats] = Eq.fromUniversalEquals
  implicit val show: Show[RunStats] = Show.fromToString
}

//maybe this one can go to meta
final case class Location(file: String, line: Int)

object Location {
  implicit val show: Show[Location] = location => show"${location.file}:${location.line}"
}
