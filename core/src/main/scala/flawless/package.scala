import cats.implicits._

import cats.effect.IO
import cats.effect.ExitCode
import cats.data.NonEmptyList
import cats.Id

package object flawless {
  type IOTest[A]   = IO[A]
  type PureTest[A] = A

  import cats.effect.Console.io._

  def getStats(specs: NonEmptyList[SuiteResult]): RunStats = {
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

  def loadArgs(args: List[String]): IO[Unit] = {
    val _ = args
    IO.unit
  }

  def runTests(args: List[String])(iotest: IOTest[NonEmptyList[SuiteResult]]) =
    loadArgs(args) >> iotest.flatMap(summarize)

  def summarize(specs: NonEmptyList[SuiteResult]): IO[ExitCode] = {
    import scala.io.AnsiColor

    val stats  = getStats(specs)
    val weGood = stats.suite.failed === 0
    val exit   = if (weGood) ExitCode.Success else ExitCode.Error

    def inGreen(s: String): String = {
      AnsiColor.GREEN + s + AnsiColor.RESET
    }

    def inRed(s: String): String = {
      AnsiColor.RED + s + AnsiColor.RESET
    }

    def inColor(test: TestResult): String = {
      val successful = test.assertions.value.forall(_.isSuccessful)
      val testName =
        if (successful) inGreen(show"Passed: ${test.name}")
        else inRed(show"Failed: ${test.name}")

      val failedAssertions = test.assertions.value.toList.collect {
        case Assertion.Failed(failure) =>
          inRed(
            show"${failure.text} (${failure.location})"
          )
      }

      testName + (if (!successful) failedAssertions.mkString("\n", "\n", "\n") else "")
    }

    val successMessage = {
      val base =
        show"Succeeded: ${stats.test.succesful} tests (${stats.assertion.succesful} assertions)"
      if (weGood) inGreen(base)
      else base
    }

    val failureMessage = {
      val base =
        show"Failed: ${stats.test.failed} tests (${stats.assertion.failed} assertions)"
      if (weGood) base
      else inRed(base)
    }

    val msg =
      show"""Ran ${stats.suite.total} suites, ${stats.test.total} tests, ${stats.assertion.total} assertions
            |$successMessage
            |$failureMessage""".stripMargin

    val showSummary = putStrLn("============ TEST SUMMARY ============")
    specs.flatMap(_.results).map(inColor).traverse(putStrLn) >> showSummary >> putStrLn(
      msg
    ).as(exit)
  }

  object syntax {
    object pure extends Dsl[Id]
    object io   extends Dsl[IO]
  }
}
