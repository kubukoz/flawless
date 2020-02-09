package flawless.eval

import cats.implicits._
import cats.Monad
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
import flawless.eval.unique.Unique
import cats.kernel.Eq
import cats.Functor
import cats.effect.Sync
import cats.Applicative

@finalAlg
trait Interpreter[F[_]] {

  /**
    * Interprets the test structure to the underlying effect. This is where all the actual execution happens.
    */
  def interpret(reporter: Reporter[F]): InterpretOne[Suite, F]
}

object Interpreter {
  //A type alias for an action that interprets a single instance of Algebra (e.g. suite or test)
  type InterpretOne[Algebra[_[_]], F[_]] = Algebra[F] => F[Algebra[NoEffect]]

  def defaultInterpreter[F[_]: Monad]: Interpreter[F] =
    new Interpreter[F] {

      private def interpretTest(implicit reporter: Reporter[F]): InterpretOne[Test, F] = { test =>
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

      private def interpretSuite(reporter: Reporter[F])(id: reporter.Identifier): InterpretOne[Suite.algebra.One, F] = {
        suite =>
          def finish(results: NonEmptyList[Test[NoEffect]]): Suite.algebra.One[NoEffect] =
            Suite.algebra.One(suite.name, results)

          reporter.publish(Reporter.Event.SuiteStarted(suite.name, id)) *>
            suite.tests.nonEmptyTraverse(interpretTest(reporter)).map(finish) <*
            reporter.publish(Reporter.Event.SuiteFinished(suite.name, id))
      }

      import Suite.algebra._

      def interpret(reporter: Reporter[F]): InterpretOne[Suite, F] = {
        def interpretOne(parentId: reporter.Identifier): InterpretOne[Suite, F] = {
          case s: Sequence[f] =>
            type IdentifiedSuites = NonEmptyList[(Suite[f], reporter.Identifier)]

            val reportSuites: IdentifiedSuites => f[Unit] = suites =>
              reporter.publish(Reporter.Event.ReplaceSuiteWith(parentId, suites.map(_._2)))

            val interpretSuites: IdentifiedSuites => f[NonEmptyList[Suite[NoEffect]]] =
              s.traversal.traverse(_) { case (suite, id) => interpretOne(id)(suite) }

            s.suites
              .traverse((reporter.ident: f[reporter.Identifier]).tupleLeft(_))
              .flatTap(reportSuites)
              .flatMap(interpretSuites)
              .map(Suite.sequence[f](_))
          case o: One[f]       => interpretSuite(reporter)(parentId)(o)
          case s: Suspend[f]   => s.suite.flatMap(interpretOne(parentId))
          case r: RResource[f] => r.resuite.use(interpretOne(parentId))(r.bracket)
        }

        s => reporter.ident.flatMap(interpretOne(_)(s))
      }
    }
}

@finalAlg
trait Reporter[F[_]] {
  type Identifier
  def ident: F[Identifier]
  def publish(event: Reporter.Event[Identifier]): F[Unit]
}

object Reporter {
  sealed trait Event[Identifier] extends Product with Serializable

  object Event {
    final case class TestStarted[Identifier](name: String) extends Event[Identifier]
    final case class TestFinished[Identifier](name: String) extends Event[Identifier]
    final case class SuiteStarted[Identifier](name: String, id: Identifier) extends Event[Identifier]
    final case class SuiteFinished[Identifier](name: String, id: Identifier) extends Event[Identifier]

    final case class ReplaceSuiteWith[Identifier](replace: Identifier, withSuites: NonEmptyList[Identifier])
      extends Event[Identifier]

    implicit def eq[Identifier]: Eq[Event[Identifier]] = Eq.fromUniversalEquals
  }

  final case class SuiteHistory(cells: /* NonEmpty */ List[SuiteHistory.Cell]) {
    def stringify: String = ???
  }

  object SuiteHistory {
    final case class Cell(status: Status)
    sealed trait Status extends Product with Serializable

    object Status {
      case class Pending(id: Unique) extends Status
      case class Running(id: Unique) extends Status
      case object Succeeded extends Status
      case object Failed extends Status
    }

    val initial: SuiteHistory = SuiteHistory(Nil)

    type MState[F[_]] = MonadState[F, SuiteHistory]
    def MState[F[_]](implicit F: MState[F]): MState[F] = F

    def replace[F[_]: MState](toRemove: Unique, cells: NonEmptyList[Cell]): F[Unit] =
      MState[F].modify(
        c =>
          c.copy(c.cells.filter {
            case Cell(Status.Pending(`toRemove`)) => false
            case _                                => true
          } ++ cells.toList)
      )

    def markRunning[F[_]: MState](id: Unique): F[Unit] = updateStatus[F] {
      case SuiteHistory.Status.Pending(`id`) => SuiteHistory.Status.Running(id)
      case unchanged                         => unchanged
    }

    def markFinished[F[_]: MState](id: Unique): F[Unit] = updateStatus[F] {
      case SuiteHistory.Status.Running(`id`) => SuiteHistory.Status.Succeeded
      case unchanged                         => unchanged
    }

    //sub-optimal map, could stop early
    //todo rename to *firstStatus when changed
    def updateStatus[F[_]: MState](update: Status => Status): F[Unit] =
      MState[F].modify { history =>
        history.copy(
          cells = history.cells.map { cell =>
            SuiteHistory.Cell(update(cell.status))
          }
        )
      }

    def show[F[_]: MState: Functor]: F[Unit] = MState[F].get.map(a => println(a)) //psst...
  }

  def consoleInstance[F[_]: Sync: ConsoleOut: SuiteHistory.MState]: Reporter[F] =
    new Reporter[F] {
      type Identifier = Unique
      val ident: F[Unique] = Sync[F].delay(new Unique)

      private def putStrWithDepth(depth: Int): String => F[Unit] = s => ConsoleOut[F].putStrLn(" " * depth * 2 + s)

      private val putSuite = putStrWithDepth(0)
      private val putTest = putStrWithDepth(1)

      def publish(event: Event[Unique]): F[Unit] = event match {
        case Event.TestStarted(name)                     => putTest(show"Starting test: $name")
        case Event.TestFinished(name)                    => putTest(show"Finished test: $name")
        case Event.SuiteStarted(name, id)                => putSuite(show"Starting suite: $name with id $id")
        case Event.SuiteFinished(name, id)               => putSuite(show"Finished suite: $name with id $id")
        case Event.ReplaceSuiteWith(toRemove, toReplace) => putSuite(show"Replacing suite $toRemove with $toReplace")
      }
    }

  def visual[F[_]: Sync: ConsoleOut: SuiteHistory.MState]: Reporter[F] = new Reporter[F] {
    type Identifier = Unique
    val ident: F[Unique] = Sync[F].delay(new Unique)

    def publish(event: Event[Identifier]): F[Unit] = event match {
      case Event.SuiteStarted(_, id)  => SuiteHistory.markRunning(id)
      case Event.SuiteFinished(_, id) => SuiteHistory.markFinished(id)
      case Event.ReplaceSuiteWith(toRemove, toReplace) =>
        val newCells: NonEmptyList[SuiteHistory.Cell] =
          toReplace.map(SuiteHistory.Status.Pending(_)).map(SuiteHistory.Cell(_))
        SuiteHistory.replace(toRemove, newCells)

      case _ => Applicative[F].unit
    }
  }
}
