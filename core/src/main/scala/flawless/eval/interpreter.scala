package flawless.eval

import cats.implicits._
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
import cats.effect.Sync
import cats.Applicative
import cats.effect.concurrent.Ref
import cats.FlatMap
import cats.MonadError
import scala.util.control.NonFatal
import monocle.macros.Lenses
import cats.data.Chain
import flawless.util.ChainUtils._

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

  def defaultInterpreter[F[_]: MonadError[*[_], Throwable]]: Interpreter[F] =
    new Interpreter[F] {

      private def interpretTest(implicit reporter: Reporter[F]): InterpretOne[Test, F] = { test =>
        def finish(results: Assertion): Test[NoEffect] = Test(test.name, TestRun.Pure(results))

        val exec: F[Test[NoEffect]] = test.result match {
          //this is a GADT skolem - you think I'd know what that means by now...
          case eval: TestRun.Eval[f] => eval.effect.handleError(Assertion.thrown(_)).map(finish)
          case TestRun.Pure(result)  => finish(result).pure[F]
          case TestRun.Lazy(e) =>
            finish {
              try e.value
              catch { case NonFatal(e) => (Assertion.thrown(e)) }
            }.pure[F]
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
            suite.tests.nonEmptyTraverse(interpretTest(reporter).apply(_)).map(finish).flatTap { suiteResult =>
              //todo duplicated logic!!!!
              val isSuccessful =
                suiteResult
                  .tests
                  .map(_.result.assertions[cats.Id])
                  .flatMap(_.results.toNonEmptyList)
                  .forall(_.isSuccessful)

              reporter.publish(Reporter.Event.SuiteFinished(suite.name, id, isSuccessful))
            }
      }

      import Suite.algebra._

      def interpret(reporter: Reporter[F]): InterpretOne[Suite, F] = {
        def interpretOne(parentId: reporter.Identifier): InterpretOne[Suite, F] = {
          case s: Sequence[f] =>
            type IdentifiedSuites = NonEmptyList[(Suite[f], reporter.Identifier)]

            val interpretSuites: IdentifiedSuites => f[NonEmptyList[Suite[NoEffect]]] =
              s.traversal.traverse(_) { case (suite, id) => interpretOne(id)(suite) }

            val suiteCount = s.suites.length

            (reporter.splitParent(parentId, suiteCount): f[NonEmptyList[reporter.Identifier]])
              .map((idents: NonEmptyList[reporter.Identifier]) => s.suites.zipWith(idents)((_, _)))
              .flatMap(interpretSuites)
              .map(Suite.sequence[f](_))
          case o: One[f]       => interpretSuite(reporter)(parentId)(o)
          case s: Suspend[f]   => s.suite.flatMap(interpretOne(parentId))
          case r: RResource[f] => r.resuite.use(interpretOne(parentId))(r.bracket)
        }

        interpretOne(reporter.root)
      }
    }
}

@finalAlg
trait Reporter[F[_]] {
  type Identifier
  def root: Identifier

  // Replaces the parent identifier with [[count]] new identifiers.
  def splitParent(parent: Identifier, count: Int): F[NonEmptyList[Identifier]]
  def publish(event: Reporter.Event[Identifier]): F[Unit]
}

object Reporter {
  type Aux[F[_], Ident] = Reporter[F] { type Identifier = Ident }

  sealed trait Event[Identifier] extends Product with Serializable

  object Event {
    final case class TestStarted[Identifier](name: String) extends Event[Identifier]
    final case class TestFinished[Identifier](name: String) extends Event[Identifier]
    final case class SuiteStarted[Identifier](name: String, id: Identifier) extends Event[Identifier]

    final case class SuiteFinished[Identifier](name: String, id: Identifier, succeeded: Boolean)
      extends Event[Identifier]

    implicit def eq[Identifier]: Eq[Event[Identifier]] = Eq.fromUniversalEquals
  }

  @Lenses
  final case class SuiteHistory(cells: Chain[SuiteHistory.Cell]) {

    //reference implementation, will be overridden for more performance (and possibly no fs2 dependency)
    def stringify: String = {
      val failedSuites = cells.count(_.status === SuiteHistory.Status.Failed)

      val failedSuiteCountOption = failedSuites.some.filter(_ > 0).map { failures =>
        show"\n${Console.RED}[$failures failed]"
      }

      fs2.Stream.emit(Console.RESET) ++
        cells.foldMap(fs2.Stream.emit(_)).groupAdjacentBy(_.status).map(_.map(_.size)).map {
          case (status, cellCount) => status.color ++ status.stringify.combineN(cellCount)
        } ++
        fs2.Stream.emits(failedSuiteCountOption.toList) ++
        fs2.Stream.emit(Console.RESET)
    }.compile.string
  }

  object SuiteHistory {

    final case class Cell(id: Unique, status: Status)

    sealed trait Status extends Product with Serializable {
      import Status._

      def stringify: String = this match {
        case Status.Pending => "▫"
        case _              => "◼"
      }

      def color: String = this match {
        case Pending   => Console.RESET
        case Running   => Console.YELLOW
        case Succeeded => Console.GREEN
        case Failed    => Console.RED
      }
    }

    object Status {
      case object Pending extends Status
      case object Running extends Status
      case object Succeeded extends Status
      case object Failed extends Status

      implicit val eq: Eq[Status] = Eq.fromUniversalEquals
    }

    def initial(rootId: Unique): SuiteHistory = SuiteHistory(Chain.one(Cell(rootId, Status.Pending)))

    type MState[F[_]] = MonadState[F, SuiteHistory]
    def MState[F[_]](implicit F: MState[F]): MState[F] = F

    def replace[F[_]: MState](toRemove: Unique, cells: NonEmptyList[Cell]): F[Unit] =
      MState[F].modify(
        SuiteHistory
          .cells
          .modify(
            flatReplaceFirst {
              case Cell(`toRemove`, Status.Pending) => Chain.fromSeq(cells.toList)
            }
          )
      )

    def markRunning[F[_]: MState](id: Unique): F[Unit] =
      setStatus(id, Status.Running)

    def markFinished[F[_]: MState](id: Unique, succeeded: Boolean): F[Unit] = {
      val newStatus = if (succeeded) SuiteHistory.Status.Succeeded else SuiteHistory.Status.Failed

      setStatus(id, newStatus)
    }

    def setStatus[F[_]: MState](id: Unique, newStatus: Status): F[Unit] = MState[F].modify {
      SuiteHistory.cells.modify {
        flatReplaceFirst {
          case cell if cell.id === id => Chain.one(cell.copy(status = newStatus))
        }
      }
    }

    def show[F[_]: MState: FlatMap: ConsoleOut]: F[Unit] =
      MState[F].get.flatMap { result =>
        val clear = "\u001b[2J\u001b[H"

        //todo: read version
        val header = "Flawless 0.1.0 👌"

        val content =
          //no `map` for laziness
          if (result.cells.exists(_.status === Status.Pending))
            result.stringify
          else "Finished"

        ConsoleOut[F].putStrLn(
          show"""$clear$header
                |
                |$content""".stripMargin
        )
      }
  }

  def consoleInstance[F[_]: Sync: ConsoleOut]: F[Reporter[F]] = Ref[F].of(0).map { identifiers =>
    new Reporter[F] {
      type Identifier = Int

      val root: Int = 0

      private val ident: F[Identifier] = identifiers.modify(a => (a + 1, a))

      def splitParent(parent: Int, count: Int): F[NonEmptyList[Int]] = {
        val ids = ident.replicateA(count).map(NonEmptyList.fromListUnsafe)

        ids.flatTap { newIds =>
          putSuite(show"Replacing suite $parent with $newIds")
        }
      }

      private def putStrWithDepth(depth: Int): String => F[Unit] = s => ConsoleOut[F].putStrLn(" " * depth * 2 + s)

      private val putSuite = putStrWithDepth(0)
      private val putTest = putStrWithDepth(1)

      def publish(event: Event[Identifier]): F[Unit] = event match {
        case Event.TestStarted(name)      => putTest(show"Starting test: $name")
        case Event.TestFinished(name)     => putTest(show"Finished test: $name")
        case Event.SuiteStarted(name, id) => putSuite(show"Starting suite: $name with id $id")
        case Event.SuiteFinished(name, id, succ) =>
          putSuite(show"Finished suite: $name with id $id. Succeeded? $succ")
      }
    }
  }

  import com.olegpy.meow.effects._

  def visual[F[_]: Sync: ConsoleOut]: F[Reporter[F]] = {
    val newId = Sync[F].delay(new Unique)

    newId.flatMap { rootIdent =>
      Ref[F].of(SuiteHistory.initial(rootIdent)).map(_.stateInstance).map { implicit S =>
        new Reporter[F] {
          type Identifier = Unique
          val root: Unique = rootIdent

          def splitParent(parent: Unique, count: Int): F[NonEmptyList[Unique]] = {
            val newIds = newId.replicateA(count).map(NonEmptyList.fromListUnsafe)

            newIds.flatTap { ids =>
              val newCells = ids.map(SuiteHistory.Cell(_, SuiteHistory.Status.Pending))
              SuiteHistory.replace[F](parent, newCells)
            }
          }

          def publish(event: Event[Identifier]): F[Unit] = {
            event match {
              case Event.SuiteStarted(_, id)        => SuiteHistory.markRunning(id)
              case Event.SuiteFinished(_, id, succ) => SuiteHistory.markFinished(id, succ)
              case _                                => Applicative[F].unit
            }
          } *> SuiteHistory.show
        }
      }
    }
  }

  def noop[F[_]: Applicative]: Reporter[F] = new Reporter[F] {
    type Identifier = Unit
    val root: Identifier = ()
    def splitParent(parent: Identifier, count: Int): F[NonEmptyList[Identifier]] = ().pure[NonEmptyList].pure[F]
    def publish(event: Event[Identifier]): F[Unit] = Applicative[F].unit
  }
}
