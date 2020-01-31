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
import cats.mtl.MonadState

@finalAlg
trait Interpreter[F[_]] {

  /**
    * Interprets the test structure to the underlying effect. This is where all the actualy execution happens.
    */
  def interpret(reporter: Reporter[F]): InterpretOne[Suite, F]
}

object Interpreter {
  //A type alias for an action that interprets a single instance of Algebra (e.g. suite or test)
  type InterpretOne[Algebra[_[_]], F[_]] = Algebra[F] => F[Algebra[NoEffect]]

  def defaultInterpreter[F[_]: Monad]: Interpreter[F] =
    new Interpreter[F] {

      private def interpretTest(reporter: Reporter[F]): InterpretOne[Test, F] = { test =>
        def finish(results: Assertion): Test[NoEffect] = Test(test.name, TestRun.Pure(results))

        val exec: F[Test[NoEffect]] = test.result match {
          //this is a GADT skolem - you think I'd know what that means by now...
          case eval: TestRun.Eval[f] => eval.effect.map(finish)
          case TestRun.Pure(result)  => finish(result).pure[F]
          case TestRun.Lazy(e)       => e.map(finish).value.pure[F]
        }

        reporter.publish(Reporter.Event.TestStarted(test.name)) *>
          exec <*
          reporter.publish(Reporter.Event.TestFinished(test.name))
      }

      private def interpretSuite(reporter: Reporter[F]): InterpretOne[Suite.algebra.One, F] = { suite =>
        def finish(results: NonEmptyList[Test[NoEffect]]): Suite.algebra.One[NoEffect] =
          Suite.algebra.One(suite.name, results)

        reporter.publish(Reporter.Event.SuiteStarted(suite.name)) *>
          suite.tests.nonEmptyTraverse(interpretTest(reporter)).map(finish) <*
          reporter.publish(Reporter.Event.SuiteFinished(suite.name))
      }

      import Suite.algebra._

      def interpret(reporter: Reporter[F]): InterpretOne[Suite, F] = {
        case s: Sequence[f]  => s.traversal.traverse(s.suites)(interpret(reporter)).map(Suite.sequence[f](_))
        case o: One[f]       => interpretSuite(reporter)(o)
        case s: Suspend[f]   => s.suite.flatMap(interpret(reporter))
        case r: RResource[f] => r.resuite.use(interpret(reporter))(r.bracket)
      }
    }
}

@finalAlg
trait Reporter[F[_]] {
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

  final case class SuiteHistory(cells: List[SuiteHistory.SuiteCell], currentSuite: Option[String]) {
    def stringify: String = ???
  }

  object SuiteHistory {
    final case class SuiteCell(status: Status)
    sealed trait Status extends Product with Serializable

    object Status {
      case object Pending extends Status
      case object Running extends Status
      case object Succeeded extends Status
      case object Failed extends Status
    }

    val initial: SuiteHistory = SuiteHistory(Nil, None)

    type MState[F[_]] = MonadState[F, SuiteHistory]
    def MState[F[_]](implicit F: MState[F]): MState[F] = F

    def addPending[F[_]: MState](count: Int): F[Unit] = {
      val pendingCell = SuiteCell(Status.Pending)

      MState[F].modify(c => c.copy(c.cells ++ List.fill(count)(pendingCell)))
    }
  }

  def consoleInstance[F[_]: FlatMap: ConsoleOut: SuiteHistory.MState]: Reporter[F] =
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

  def visual[F[_]: FlatMap: ConsoleOut: SuiteHistory.MState]: Reporter[F] = ???
}
