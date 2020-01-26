package flawless.eval

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
import flawless.NoEffect
import cats.data.NonEmptyList

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

      private val interpretTest: InterpretOne[Test, F] = { test =>
        def finish(results: Assertion): Test[NoEffect] = Test(test.name, TestRun.Pure(results))

        val exec: F[Test[NoEffect]] = test.result match {
          //this is a GADT skolem - you think I'd know what that means by now...
          case eval: TestRun.Eval[f] => eval.effect.map(finish)
          case TestRun.Pure(result)  => finish(result).pure[F]
          case TestRun.Lazy(e)       => e.map(finish).value.pure[F]
        }

        Reporter[F].publish(Reporter.Event.TestStarted(test.name)) *>
          exec <*
          Reporter[F].publish(Reporter.Event.TestFinished(test.name))
      }

      private val interpretSuite: InterpretOne[Suite.algebra.One, F] = { suite =>
        def finish(results: NonEmptyList[Test[NoEffect]]): Suite.algebra.One[NoEffect] =
          Suite.algebra.One(suite.name, results)

        Reporter[F].publish(Reporter.Event.SuiteStarted(suite.name)) *>
          suite.tests.nonEmptyTraverse(interpretTest).map(finish) <*
          Reporter[F].publish(Reporter.Event.SuiteFinished(suite.name))
      }

      import Suite.algebra._

      val interpret: InterpretOne[Suite, F] = {
        case s: Sequence[f]  => s.traversal.traverse(s.suites)(interpret).map(Suite.sequence[f](_))
        case o: One[f]       => interpretSuite(o)
        case s: Suspend[f]   => s.suite.flatMap(interpret)
        case r: RResource[f] => r.resuite.use(interpret)(r.bracket)
      }
    }
}

@finalAlg
trait Reporter[F[_]] {
  // def reportTest: InterpretOne[Test, F] => InterpretOne[Test, F]
  // def reportSuite: InterpretOne[Suite.algebra.One, F] => InterpretOne[Suite.algebra.One, F]

  def publish(event: Reporter.Event): F[Unit]
}

object Reporter {
  sealed trait Event extends Product with Serializable

  object Event {
    final case class TestStarted(name: String) extends Event
    final case class TestFinished(name: String) extends Event
    final case class SuiteStarted(name: String) extends Event
    final case class SuiteFinished(name: String) extends Event
  }

  def consoleInstance[F[_]: FlatMap: ConsoleOut]: Reporter[F] =
    new Reporter[F] {
      private def putStrWithDepth(depth: Int): String => F[Unit] = s => ConsoleOut[F].putStrLn(" " * depth * 2 + s)

      private val putSuite = putStrWithDepth(0)
      private val putTest = putStrWithDepth(1)

      def publish(event: Event): F[Unit] = event match {
        case Event.TestStarted(name)   => putTest("Starting test: " + name)
        case Event.TestFinished(name)  => putTest("Finished test: " + name)
        case Event.SuiteStarted(name)  => putSuite("Starting suite: " + name)
        case Event.SuiteFinished(name) => putSuite("Finished suite: " + name)
      }
    }
}
