package flawless

import cats.effect.ExitCode
import cats.Id
import cats.implicits._
import flawless.data.Assertion.Result.Pending
import cats.data.NonEmptyList

package object eval {
  import flawless.data.Assertion.Result.Failed

  import cats.Applicative

  //todo: show names of suites
  def toTerminalOutput(output: Output): (String, ExitCode) = {
    import scala.io.AnsiColor

    val stats = output.stats

    val weGood = stats.suite.failed === 0
    val exit = if (weGood) ExitCode.Success else ExitCode.Error

    def inSuccessful(s: String): String =
      AnsiColor.GREEN + s + AnsiColor.RESET

    def inFailed(s: String): String =
      AnsiColor.RED + s + AnsiColor.RESET

    def inPending(s: String): String =
      AnsiColor.MAGENTA + s + AnsiColor.RESET

    val msg = {
      val header = show"Ran ${stats.suite.total} suites, ${stats.test.total} tests, ${stats.assertion.total} assertions"

      def summary(getValue: RunStats.Stat => Int): String =
        show"${getValue(stats.suite)} suites ${getValue(stats.test)} tests (${getValue(stats.assertion)} assertions)"

      val successMessage = {
        val base = show"Succeeded: ${summary(_.successful)}"
        if (weGood) inSuccessful(base)
        else base
      }

      val pendingMessage = {
        val base = show"Pending: ${summary(_.pending)}"

        val anythingPending = NonEmptyList
          .of(
            stats.suite.pending,
            stats.test.pending,
            stats.assertion.pending
          )
          .exists(_ > 0)

        anythingPending.guard[Option].as(base).map(inPending)
      }

      val failureMessage = {
        val base = show"Failed: ${summary(_.failed)}"
        if (weGood) base
        else inFailed(base)
      }

      List(header, successMessage) ++
        pendingMessage.toList ++
        List(failureMessage)
    }.mkString("\n")

    def inColor(test: Output.Test): String = test.problems match {
      //todo here
      case Left(problems) =>
        val (isPending, errors) = problems.toList.partitionEither(_.toEither).bimap(_.nonEmpty, _.toNel)

        val pendingString = isPending.guard[Option].as(inPending(show"Pending: ${test.name}"))

        val errorString = errors.map(_.mkString_("\n", "\n", "\n")).map(show"Failed: ${test.name}" ++ _).map(inFailed)

        List(errorString, pendingString).flattenOption.mkString("\n")

      case Right(_) => inSuccessful(show"Passed: ${test.name}")
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
      show"""${Constants.clear}$results============ TEST SUMMARY ============
            |$msg
            |""".stripMargin

    (outputString, exit)
  }

  def summarize(suites: Suite[Id]): Output = {
    val suitesFlat = data.Suite.flatten(suites)

    val stats = RunStats.fromSuites(suites)

    val suiteOutputs = suitesFlat.map(convertSuite)

    Output(stats, suiteOutputs)
  }

  private def convertSuite(suite: Suite.algebra.One[Id]): Output.Suite = {
    val testOutputs = suite.tests.map { test =>
      val problems = test
        .result
        .assertions
        .results
        .collect {
          case Failed(message) => Output.Problem.Failed(message)
          case Pending         => Output.Problem.Pending
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
