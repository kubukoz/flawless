package flawless.stats

import cats.{Eq, Foldable, Show}
import cats.implicits._
import cats.data.NonEmptyList
import flawless.{Assertion, SuiteResult, TestResult}
import monocle.{Lens, Traversal}
import monocle.macros.GenLens

case class RunStats(
  suite: RunStats.Stat,
  test: RunStats.Stat,
  assertion: RunStats.Stat
)

object RunStats {

  def fromSuites(suites: NonEmptyList[SuiteResult]): RunStats = {
    val stats = getStats(suites.toList)

    val suiteStat     = stats.of(Traversal.id)(optics.suiteToAssertions)
    val testStat      = stats.of(optics.suiteToTests)(optics.testToAssertions)
    val assertionStat = stats.of(optics.suiteToAssertions)(Traversal.id)

    RunStats(
      suite = suiteStat,
      test = testStat,
      assertion = assertionStat
    )
  }

  //Lenses/traversals for suite/test/assertion results
  private object optics {
    private val suiteTests: Lens[SuiteResult, NonEmptyList[TestResult]] =
      GenLens[SuiteResult](_.results)

    private val testAssertions: Lens[TestResult, NonEmptyList[Assertion]] =
      GenLens[TestResult](_.assertions.value)

    val suiteToTests: Traversal[SuiteResult, TestResult] =
      suiteTests.composeTraversal(Traversal.fromTraverse)

    val testToAssertions: Traversal[TestResult, Assertion] =
      testAssertions.composeTraversal(Traversal.fromTraverse)

    val suiteToAssertions: Traversal[SuiteResult, Assertion] = optics.suiteToTests >>> optics.testToAssertions
  }

  /**
    * For each element `a` in `fa`, partition all the selected elements found in `a` by the given predicate.
    *
    * @param fa the root container (always the same on the same tree)
    * @param select the way to select all the elements you want to end up in the list
    * @param p the predicate that checks the elements
    * @return (matching, nonmatching)
    */
  private def partitionByAll[F[_]: Foldable, Root, Selected](
    fa: F[Root]
  )(
    select: Traversal[Root, Selected]
  )(p: Selected => Boolean): (List[Selected], List[Selected]) = {
    fa.foldMap(select.getAll(_).partition(p))
  }

  private def getStats[F[_]: Foldable](
    suites: F[SuiteResult]
  ) = new GetStatsPartiallyApplied(suites)

  class GetStatsPartiallyApplied[F[_]: Foldable] private[RunStats] (suites: F[SuiteResult]) {

    /**
      * Get the stats for the selected metric (as defined by the `select` traversal) of all the suites in `fa`.
      * `traversal` defines how to go from the selected metric to the assertions.
      * */
    def of[Selected](
      select: Traversal[SuiteResult, Selected]
    )(traversal: Traversal[Selected, Assertion]): RunStats.Stat = {
      val (succeeded, failed) = partitionByAll(suites)(select)(traversal.all(_.isSuccessful))

      val successCount = succeeded.size
      val failureCount = failed.size

      RunStats.Stat(failureCount + successCount, successCount, failureCount)
    }
  }

  case class Stat(total: Int, successful: Int, failed: Int)

  implicit val eq: Eq[RunStats]     = Eq.fromUniversalEquals
  implicit val show: Show[RunStats] = Show.fromToString
}

//maybe this one can go to meta
case class Location(file: String, line: Int)

object Location {
  implicit val show: Show[Location] = location => show"${location.file}:${location.line}"
}
