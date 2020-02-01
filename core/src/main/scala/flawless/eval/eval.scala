package flawless

import cats.effect.ConsoleOut
import cats.FlatMap
import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.Id
import cats.implicits._
import flawless.data.Test

package object eval {

  import cats.Applicative

  def summarize[F[_]: ConsoleOut: FlatMap](suites: Suite[Id]): F[ExitCode] = {
    import scala.io.AnsiColor
    val suitesFlat = data.Suite.flatten(suites)

    val stats = RunStats.fromSuites[NonEmptyList](suitesFlat)

    val weGood = stats.suite.failed === 0
    val exit = if (weGood) ExitCode.Success else ExitCode.Error

    def inGreen(s: String): String =
      AnsiColor.GREEN + s + AnsiColor.RESET

    def inRed(s: String): String =
      AnsiColor.RED + s + AnsiColor.RESET

    def inColor(test: Test[Id]): String = {
      val assertions = test.result.assertions.flatMap(_.results)

      val successful = assertions.forall(_.isSuccessful)
      val testName =
        if (successful) inGreen(show"Passed: ${test.name}")
        else inRed(show"Failed: ${test.name}")

      val failedAssertions = assertions.toList.collect {
        case flawless.data.Assertion.Result.Failed(failure) =>
          inRed(
            // show"${failure.text} (${failure.location})"
            failure //todo
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

    val showSummary = ConsoleOut[F].putStrLn("============ TEST SUMMARY ============")

    val showResults = suitesFlat.flatMap(_.tests).map(inColor).nonEmptyTraverse_(ConsoleOut[F].putStrLn)

    showResults *>
      showSummary *>
      ConsoleOut[F].putStrLn(msg).as(exit)
  }

  def loadArgs[F[_]: Applicative](args: List[String]): F[Settings] = {
    val _ = args
    //todo actually read the args
    Settings(visual = true).pure[F]
    Settings(visual = false).pure[F]
    // Settings(visual = args.contains("visual")).pure[F]
  }
}
