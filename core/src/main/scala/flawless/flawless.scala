import cats.effect.ExitCode
import cats.implicits._
import flawless.stats.RunStats
import cats.Id
import flawless.data.Assertion
import cats.data.NonEmptyList
import cats.FlatMap
import flawless.data.Test
import cats.Monad
import cats.Applicative
import cats.effect.ConsoleOut

package object flawless {

  // Shamelessly ripped off from fs2's Pure type - this is pure genius.
  type NoEffect[A] <: Nothing

  type Suite[+F[_]] = flawless.data.Suite[F]
  val Suite = flawless.data.Suite

  // This name is bad (Predicate implies A => Boolean). Come up with a better name.
  // Possibly worth newtyping.
  // This idea is heavily inspired by ZIO Test.
  type Predicate[-A] = A => Assertion

  object syntax extends api.AllDsl with api.AllPredicates
  object dsl extends api.AllDsl
  object predicates extends api.AllPredicates

  def loadArgs[F[_]: Applicative](args: List[String]): F[Unit] = {
    val _ = args
    Applicative[F].unit
  }

  def runTests[F[_]: Interpreter: ConsoleOut: Monad](args: List[String])(suites: Suite[F]): F[ExitCode] =
    loadArgs[F](args) *> suites.interpret.flatMap(summarize[F])

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
}
