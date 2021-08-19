package flawless

import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.Id
import cats.implicits._

package object eval {
  import flawless.data.Assertion.Result.Failed
  import cats.Applicative

  //todo: show names of suites
  def toTerminalOutput(output: Output): (String, ExitCode) = {
    import scala.io.AnsiColor

    val stats = output.stats

    val weGood = stats.suite.failed === 0
    val exit = if (weGood) ExitCode.Success else ExitCode.Error

    def inGreen(s: String): String =
      AnsiColor.GREEN + s + AnsiColor.RESET

    def inRed(s: String): String =
      AnsiColor.RED + s + AnsiColor.RESET

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

    def inColor(test: Output.Test): String = test.problems match {
      case Left(problems) => inRed(show"Failed: ${test.name}" ++ problems.mkString_("\n", "\n", "\n"))
      case Right(_)       => inGreen(show"Passed: ${test.name}")
    }

    val results =
      output
        .suites
        .flatMap { suite =>
          suite.name ::
            (suite.tests.map(inColor) :+ "")
        }
        .mkString_("\n", "\n", "\n")

    val outputString =
      show"""$results============ TEST SUMMARY ============
            |$msg
            |""".stripMargin

    (outputString, exit)
  }

  def summarize(suites: Suite[Id]): Output = {
    val suitesFlat = data.Suite.flatten(suites)

    val stats = RunStats.fromSuites[NonEmptyList](suitesFlat)

    val suiteOutputs = suitesFlat.map(convertSuite)

    Output(stats, suiteOutputs)
  }

  private def convertSuite(suite: Suite.algebra.One[Id]): Output.Suite = {
    val testOutputs = suite.tests.map { test =>
      val problems = test
        .result
        .assertions
        .results
        .collect { case Failed(message) =>
          message
        }
        .toList

      Output.Test(test.name, problems.toNel.toLeft(()))
    }

    Output.Suite(suite.name, testOutputs)
  }

  def loadArgs[F[_]: Applicative](args: List[String]): F[Settings] = {
    val _ = args
    //todo actually read the args
    Settings(visual = true).pure[F]
    // Settings(visual = false).pure[F]
    // Settings(visual = args.contains("visual")).pure[F]
  }

}
