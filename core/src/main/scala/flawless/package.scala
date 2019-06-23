import cats.implicits._

import cats.effect.IO
import cats.effect.ExitCode
import cats.data.NonEmptyList
import flawless.stats.RunStats

package object flawless {
  import cats.effect.ContextShift
  import cats.effect.Console.io._

  def loadArgs(args: List[String]): IO[Unit] = {
    val _ = args
    IO.unit
  }

  def runTests(args: List[String])(iotest: Tests.TTest[NonEmptyList[SuiteResult]])(implicit cs: ContextShift[IO]) =
    loadArgs(args) >> iotest.interpret.flatMap(summarize)

  def summarize(specs: NonEmptyList[SuiteResult]): IO[ExitCode] = {
    import scala.io.AnsiColor

    val stats = RunStats.fromSuites(specs)

    val weGood = stats.suite.failed === 0
    val exit = if (weGood) ExitCode.Success else ExitCode.Error

    def inGreen(s: String): String =
      AnsiColor.GREEN + s + AnsiColor.RESET

    def inRed(s: String): String =
      AnsiColor.RED + s + AnsiColor.RESET

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

    val msg = {
      val successMessage = {
        val base =
          show"Succeeded: ${stats.suite.successful} suites ${stats.test.successful} tests (${stats.assertion.successful} assertions)"
        if (weGood) inGreen(base)
        else base
      }

      val failureMessage = {
        val base =
          show"Failed: ${stats.suite.failed} suites ${stats.test.failed} tests (${stats.assertion.failed} assertions)"
        if (weGood) base
        else inRed(base)
      }

      show"""Ran ${stats.suite.total} suites, ${stats.test.total} tests, ${stats.assertion.total} assertions
            |$successMessage
            |$failureMessage""".stripMargin
    }

    val showSummary = putStrLn("============ TEST SUMMARY ============")

    val showResults = specs.flatMap(_.results).map(inColor).traverse(putStrLn)

    showResults >>
      showSummary >>
      putStrLn(msg).as(exit)
  }
}
