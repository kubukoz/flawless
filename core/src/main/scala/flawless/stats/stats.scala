package flawless.stats

import cats.Eq
import cats.Show
import cats.implicits._
import cats.data.NonEmptyList
import flawless.SuiteResult

case class RunStats(
  suite: RunStats.Stat,
  test: RunStats.Stat,
  assertion: RunStats.Stat
)

object RunStats {

  def fromSpecs(specs: NonEmptyList[SuiteResult]): RunStats = {
    val suiteCount = specs.size
    val tests      = specs.flatMap(_.results)
    val assertions = tests.flatMap(_.assertions.value)

    val (successfulSuites, failedSuites) =
      specs.toList.partition(_.results.forall(_.assertions.value.forall(_.isSuccessful)))
    val (successfulTests, failedTests) =
      tests.toList.partition(_.assertions.value.forall(_.isSuccessful))
    val (successfulAssertions, failedAssertions) =
      assertions.toList.partition(_.isSuccessful)

    val successfulSuiteCount = successfulSuites.size
    val failedSuiteCount     = failedSuites.size
    val testCount            = tests.size
    val successfulTestCount  = successfulTests.size
    val failedTestCount      = failedTests.size
    val assertionCount       = assertions.size
    val successCount         = successfulAssertions.size
    val failureCount         = failedAssertions.size

    RunStats(
      suite = RunStats.Stat(suiteCount, successfulSuiteCount, failedSuiteCount),
      test = RunStats.Stat(testCount, successfulTestCount, failedTestCount),
      assertion = RunStats.Stat(assertionCount, successCount, failureCount)
    )
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
