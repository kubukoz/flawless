import cats.implicits._

import cats.effect.IO
import cats.effect.ExitCode
import cats.data.Kleisli
import cats.data.NonEmptyList
import cats.Id

package object flawless {
  type IOTest[A]   = Kleisli[IO, TestRun, A]
  type PureTest[A] = Kleisli[Id, TestRun, A]

  import cats.effect.Console.io._

  def getStats(specs: NonEmptyList[SuiteResult]): RunStats = {
    val suiteCount = specs.size
    val tests      = specs.flatMap(_.results)
    val assertions = tests.flatMap(_.output.outcomes)

    val (successfulSuites, failedSuites) =
      specs.toList.partition(_.results.forall(_.output.outcomes.forall(_.isSuccessful)))
    val (successfulTests, failedTests) =
      tests.toList.partition(_.output.outcomes.forall(_.isSuccessful))
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

  def loadArgs(args: List[String]): IO[TestRun] = IO.pure(TestRun(Nil, Nil))

  def runTests(args: List[String])(iotest: IOTest[NonEmptyList[SuiteResult]]) =
    loadArgs(args).flatMap(iotest.run).flatMap(summarize)

  def summarize(specs: NonEmptyList[SuiteResult]): IO[ExitCode] = {

    val stats  = getStats(specs)
    val weGood = stats.suite.failed === 0
    val exit   = if (weGood) ExitCode.Success else ExitCode.Error

    val msg =
      show"""Ran ${stats.suite.total} suites, ${stats.test.total} tests, ${stats.assertion.total} assertions
            |Succeeded: ${stats.test.succesful} tests (${stats.assertion.succesful} assertions)
            |Failed: ${stats.test.failed} tests (${stats.assertion.failed} assertions)""".stripMargin

    putStrLn(msg).as(exit)
  }

  object syntax {
    object pure extends Dsl[Id]
    object io   extends Dsl[IO]
  }
}
