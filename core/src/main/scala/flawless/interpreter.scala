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
import cats.tagless.finalAlg
import cats.effect.ConsoleOut
import Interpreter.InterpretOne
import flawless.data.Suites.Suspend

@finalAlg
trait Interpreter[F[_]] {

  /**
    * Interprets the test structure to the underlying effect. This is where all the actualy execution happens.
    */
  def interpret: InterpretOne[Suites, F]
}

object Interpreter {
  //A type alias for an action that interprets a single instance of Algebra (e.g. suite or test)
  type InterpretOne[Algebra[_[_]], F[_]] = Algebra[F] => F[Algebra[Id]]

  implicit def defaultInterpreter[F[_]: Monad: Reporter]: Interpreter[F] =
    new Interpreter[F] {

      private val interpretTest: InterpretOne[Test, F] = { test =>
        def finish(results: NonEmptyList[Assertion]): Test[Id] = Test(test.name, TestRun.Pure(results))

        test.result match {
          //this is a GADT skolem - you think I'd know what that means by now...
          case eval: TestRun.Eval[f] => eval.effect.map(finish)
          case TestRun.Pure(result)  => finish(result).pure[F]
          case TestRun.Lazy(e)       => e.map(finish).value.pure[F]
        }
      }

      private val interpretSuite: InterpretOne[Suite, F] = suite =>
        suite.tests.nonEmptyTraverse(Reporter[F].reportTest(interpretTest)).map(Suite[Id](suite.name, _))

      val interpret: InterpretOne[Suites, F] = {
        case Sequence(suites, traversal) => traversal.traverse(suites)(interpret).map(Suites.sequence(_))
        case One(suite)                  => Reporter[F].reportSuite(interpretSuite)(suite).map(One(_))
        case Suspend(suites)             => suites.flatMap(interpret)
        case RResource(suites, bracket)  => suites.use(interpret)(bracket)
      }
    }
}

@finalAlg
trait Reporter[F[_]] {
  def reportTest: InterpretOne[Test, F] => InterpretOne[Test, F]
  def reportSuite: InterpretOne[Suite, F] => InterpretOne[Suite, F]
}

object Reporter {

  def consoleInstance[F[_]: FlatMap: ConsoleOut]: Reporter[F] =
    new Reporter[F] {
      private def putStrWithDepth(depth: Int): String => F[Unit] = s => ConsoleOut[F].putStrLn(" " * depth * 2 + s)

      private val putSuite = putStrWithDepth(0)
      private val putTest = putStrWithDepth(1)

      //this is going to need access to a summarizer of tests,
      //so that it can display the amount of assertions that succeeded, failed, etc., with colors
      val reportTest: (Test[F] => F[Test[Id]]) => Test[F] => F[Test[Id]] = interpret =>
        test =>
          for {
            _      <- putTest("Starting test: " + test.name)
            result <- interpret(test)
            _      <- putTest("Finished test: " + test.name + s", result: ${result.result}")
          } yield result

      val reportSuite: (Suite[F] => F[Suite[Id]]) => Suite[F] => F[Suite[Id]] = interpret =>
        suite =>
          for {
            _      <- putSuite("Starting suite: " + suite.name)
            result <- interpret(suite)
            _      <- putSuite("Finished suite: " + suite.name + s", result: ${result.tests}")
          } yield result
    }
}
