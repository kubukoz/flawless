package flawlessly

import flawless.data.Suite
import flawless.dsl._
import flawless._
import cats.implicits._
import cats.data.Chain
import flawless.eval.Reporter
import flawless.eval.Interpreter
import cats.Show
import cats.effect.Resource
import cats.effect.Sync
import cats.Foldable
import flawless.data.Assertion
import cats.data.NonEmptyList
import cats.Parallel
import cats.mtl.MonadState
import cats.mtl.FunctorTell
import cats.mtl.instances.all._
import cats.Alternative
import cats.Monad
import cats.data.`package`.ReaderWriterStateT

//Sync, because Bracket for WriterT isn't explicitly written
object InterpreterReportingTest {

  def apply[F[_]: Sync]: Suite[F] = {
    sealed trait LogEvent extends Product with Serializable

    object LogEvent {
      case class ReplaceWith(parent: Int, childCount: Int) extends LogEvent
      case class Report(event: Reporter.Event[Int]) extends LogEvent
      implicit val show: Show[LogEvent] = Show.fromToString
    }

    type WC[A] = ReaderWriterStateT[F, Unit, Chain[LogEvent], Int, A]

    //The instance shall not be used for parallelism! It's pretty much just a marker
    implicit val parallelState: Parallel[WC] = Parallel.identity

    import Sync.catsReaderWriteStateTSync

    val reporter: Reporter.Aux[WC, Int] = {
      def make[
        M[_]: MonadState[*[_], Int]: Monad: FunctorTell[*[_], S[LogEvent]],
        S[_]: Alternative
      ]: Reporter.Aux[M, Int] =
        new Reporter[M] {
          type Identifier = Int
          val root: Int = 0

          private val logger = FunctorTell[M, S[LogEvent]]

          private val ident: M[Identifier] = MonadState[M, Int].modify(_ + 1) *> MonadState[M, Int].get

          def splitParent(parent: Int, count: Int): M[NonEmptyList[Int]] =
            logger.tell((LogEvent.ReplaceWith(parent, count): LogEvent).pure[S]) *> ident
              .replicateA(count)
              .map(NonEmptyList.fromListUnsafe)

          def publish(event: Reporter.Event[Identifier]): M[Unit] =
            logger.tell((LogEvent.Report(event): LogEvent).pure[S])
        }

      make[WC, Chain]
    }

    val interpreter: Interpreter[WC] = Interpreter.defaultInterpreter[WC]

    implicit val showEvent: Show[Reporter.Event[Int]] = Show.fromToString

    val simpleSuite = suite[WC]("suite 1") {
      tests(
        pureTest("test 1")(ensureEqual(1, 1)),
        pureTest("test 2")(ensureEqual(1, 1))
      )
    }

    import Reporter.Event._

    def simpleEvents(id: Int): List[LogEvent] =
      List[Reporter.Event[Int]](
        SuiteStarted("suite 1", id),
        TestStarted("test 1"),
        TestFinished("test 1"),
        TestStarted("test 2"),
        TestFinished("test 2"),
        SuiteFinished("suite 1", id, succeeded = true)
      ).map(LogEvent.Report(_))

    def simpleResource(suite: Suite[WC]): Suite[WC] = Suite.resource(suite.pure[Resource[WC, *]])

    def ensureReported[G[_]: Foldable](suite: Suite[WC])(expectedWritten: G[LogEvent]): F[Assertion] =
      interpreter
        .interpret(reporter)(suite)
        .written
        .runA((), 0)
        .map(_.toList)
        .map(ensureEqual(_, expectedWritten.toList))

    //todo: these would be good property tests
    suite("InterpreterReportingTest") {
      tests(
        test("single suite") {
          ensureReported(simpleSuite)(simpleEvents(0))
        },
        test("sequence of suites") {
          ensureReported {
            Suite.sequential(simpleSuite, simpleSuite)
          } {
            List(LogEvent.ReplaceWith(0, 2)) ++
              simpleEvents(1) ++
              simpleEvents(2)
          }
        },
        test("nested sequence of suites - left side") {
          ensureReported {
            Suite.sequential(Suite.sequential(simpleSuite), simpleSuite, simpleSuite)
          } {
            List(LogEvent.ReplaceWith(0, 3)) ++
              simpleEvents(1) ++
              simpleEvents(2) ++
              simpleEvents(3)
          }
        },
        test("nested sequence of suites - right side") {
          ensureReported {
            Suite.sequential(simpleSuite, simpleSuite, Suite.sequential(simpleSuite))
          } {
            List(LogEvent.ReplaceWith(0, 3)) ++
              simpleEvents(1) ++
              simpleEvents(2) ++
              simpleEvents(3)
          }
        },
        test("nested sequence of suites - within parallel node") {
          ensureReported {
            Suite.parallel(
              simpleSuite,
              simpleSuite,
              Suite.sequential(
                Suite.sequential(
                  Suite.sequential(simpleSuite),
                  simpleSuite
                ),
                Suite.sequential(simpleSuite, simpleSuite)
              )
            )
          } {
            List(LogEvent.ReplaceWith(0, 3)) ++
              simpleEvents(1) ++
              simpleEvents(2) ++
              List(LogEvent.ReplaceWith(3, 4)) ++
              simpleEvents(4) ++
              simpleEvents(5) ++
              simpleEvents(6) ++
              simpleEvents(7)
          }
        },
        test("nested sequence of suites - both sides") {
          ensureReported {
            Suite.sequential(Suite.sequential(simpleSuite), Suite.sequential(simpleSuite))
          } {
            List(LogEvent.ReplaceWith(0, 2)) ++
              simpleEvents(1) ++
              simpleEvents(2)
          }
        },
        test("resource doesn't allocate extra suites") {
          ensureReported(simpleResource(simpleSuite))(simpleEvents(0))
        },
        test("sequence in resource") {
          ensureReported {
            Suite.sequential(
              simpleSuite,
              simpleSuite,
              simpleResource {
                Suite.sequential(
                  simpleSuite,
                  simpleSuite
                )
              }
            )
          } {
            List(
              LogEvent.ReplaceWith(0, 3).pure[List],
              simpleEvents(1),
              simpleEvents(2),
              LogEvent.ReplaceWith(3, 2).pure[List],
              simpleEvents(4),
              simpleEvents(5)
            ).flatten
          }
        }
      )
    }
  }
}
