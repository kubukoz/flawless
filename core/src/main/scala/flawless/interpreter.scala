package flawless

import flawless.data.Suites

import cats.data.NonEmptyList
import cats.implicits._
import cats.Id
import flawless.data.Suites.Sequence
import flawless.data.Suites.One
import cats.Monad
import cats.FlatMap
import flawless.data.Suites.RResource
import flawless.data.Test
import flawless.data.Assertion
import flawless.data.TestRun
import flawless.data.Suite
import flawless.data.Traversal
import cats.tagless.finalAlg
import cats.effect.ConsoleOut

@finalAlg
trait Interpreter[F[_]] {

  /**
    * Interprets the test structure to the underlying effect. This is where all the actualy execution happens.
    */
  def interpret: Suites[F] => F[Suites[Id]]
}

object Interpreter {
  implicit def defaultInterpreter[F[_]: Monad: Reporter]: Interpreter[F] =
    new Interpreter[F] {
      private val interpretTest: Test[F] => F[Test[Id]] = { test =>
        def finish(results: NonEmptyList[Assertion]): Test[Id] = Test(test.name, TestRun.Pure(results))

        test.result match {
          //this is a GADT skolem - you think I'd know what that means by now...
          case eval: TestRun.Eval[f] => eval.effect.map(finish)
          case TestRun.Pure(result)  => finish(result).pure[F]
          case TestRun.Lazy(e)       => e.map(finish).value.pure[F]
        }
      }

      //todo tests in a suite should have multiple methods of traversal
      private val interpretSuite: Suite[F] => F[Suite[Id]] = suite =>
        suite.tests.nonEmptyTraverse(Reporter[F].reportTest(interpretTest)).map(Suite[Id](suite.name, _))

      val interpret: Suites[F] => F[Suites[Id]] = {
        case Sequence(suites, traversal) =>
          val interpreted = traversal.traverse(suites) {
            //this interpret call will make sure every spec starts with a clean depth scope - watch this space
            interpret
          }

          interpreted.map(Sequence(_, Traversal.identity))
        case One(suite) => Reporter[F].reportSuite(interpretSuite)(suite).map(One(_))
        case RResource(suites, b) =>
          implicit val bracket = b
          suites.use(interpret)
      }
    }
}

@finalAlg
trait Reporter[F[_]] {
  def reportTest: (Test[F] => F[Test[Id]]) => Test[F] => F[Test[Id]]
  def reportSuite: (Suite[F] => F[Suite[Id]]) => Suite[F] => F[Suite[Id]]
}

object Reporter {

  def consoleInstance[F[_]: FlatMap: ConsoleOut]: Reporter[F] =
    new Reporter[F] {

      private def putStrWithDepth(depth: Int): String => F[Unit] = s => ConsoleOut[F].putStrLn(" " * depth * 2 + s)

      private val putSuite = ConsoleOut[F].putStrLn(_: String)
      private val putTest = putStrWithDepth(1)

      val reportTest: (Test[F] => F[Test[Id]]) => Test[F] => F[Test[Id]] = run =>
        test =>
          //this is going to need access to a summarizer of tests,
          //so that it can display the amount of assertions that succeeded, failed, etc., with colors
          putTest("Starting test: " + test.name) *> run(test).flatTap { result =>
            putTest("Finished test: " + test.name + s", result: ${result.result}")
          }

      val reportSuite: (Suite[F] => F[Suite[Id]]) => Suite[F] => F[Suite[Id]] = run =>
        suite =>
          putSuite("Starting suite: " + suite.name) *> run(suite).flatTap { result =>
            putSuite("Finished suite: " + suite.name + s", result: ${result.tests}")
          }
    }
}
