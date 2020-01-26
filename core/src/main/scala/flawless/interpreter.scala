package flawless

import cats.implicits._
import cats.Monad
import cats.FlatMap
import flawless.data.Test
import flawless.data.Assertion
import flawless.data.TestRun
import cats.tagless.finalAlg
import cats.effect.ConsoleOut
import Interpreter.InterpretOne
import flawless.data.Suite
import flawless.dsl.NoEffect

@finalAlg
trait Interpreter[F[_]] {

  /**
    * Interprets the test structure to the underlying effect. This is where all the actualy execution happens.
    */
  def interpret: InterpretOne[Suite, F]
}

object Interpreter {
  //A type alias for an action that interprets a single instance of Algebra (e.g. suite or test)
  type InterpretOne[Algebra[_[_]], F[_]] = Algebra[F] => F[Algebra[NoEffect]]

  implicit def defaultInterpreter[F[_]: Monad: Reporter]: Interpreter[F] =
    new Interpreter[F] {

      val repo = Reporter[F]

      private val interpretTest: InterpretOne[Test, F] = { test =>
        def finish(results: Assertion): Test[NoEffect] = Test(test.name, TestRun.Pure(results))

        test.result match {
          //this is a GADT skolem - you think I'd know what that means by now...
          case eval: TestRun.Eval[f] => eval.effect.map(finish)
          case TestRun.Pure(result)  => finish(result).pure[F]
          case TestRun.Lazy(e)       => e.map(finish).value.pure[F]
        }
      }

      private val interpretSuite: InterpretOne[Suite.algebra.One, F] = suite =>
        suite
          .tests
          .nonEmptyTraverse(Reporter[F].reportTest(interpretTest))
          .map(Suite.algebra.One[NoEffect](suite.name, _))

      import Suite.algebra._

      val interpret: InterpretOne[Suite, F] = {
        case s: Sequence[f]  => s.traversal.traverse(s.suites)(interpret).map(Suite.sequence[f](_))
        case o: One[f]       => repo.reportSuite(interpretSuite)(o)
        case s: Suspend[f]   => s.suite.flatMap(interpret)
        case r: RResource[f] => r.resuite.use(interpret)(r.bracket)
      }
    }
}

@finalAlg
trait Reporter[F[_]] {
  def reportTest: InterpretOne[Test, F] => InterpretOne[Test, F]
  def reportSuite: InterpretOne[Suite.algebra.One, F] => InterpretOne[Suite.algebra.One, F]
}

object Reporter {

  def consoleInstance[F[_]: FlatMap: ConsoleOut]: Reporter[F] =
    new Reporter[F] {
      private def putStrWithDepth(depth: Int): String => F[Unit] = s => ConsoleOut[F].putStrLn(" " * depth * 2 + s)

      private val putSuite = putStrWithDepth(0)
      private val putTest = putStrWithDepth(1)

      //this is going to need access to a summarizer of tests,
      //so that it can display the amount of assertions that succeeded, failed, etc., with colors
      val reportTest: (Test[F] => F[Test[NoEffect]]) => Test[F] => F[Test[NoEffect]] = interpret =>
        test =>
          for {
            _      <- putTest("Starting test: " + test.name)
            result <- interpret(test)
            _      <- putTest("Finished test: " + test.name + s", result: ${result.result}")
          } yield result

      import Suite.algebra.One

      val reportSuite: (One[F] => F[One[NoEffect]]) => One[F] => F[One[NoEffect]] =
        interpret =>
          suite =>
            for {
              _      <- putSuite("Starting suite: " + suite.name)
              result <- interpret(suite)
              _      <- putSuite("Finished suite: " + suite.name + s", result: ${result.tests}")
            } yield result
    }
}
