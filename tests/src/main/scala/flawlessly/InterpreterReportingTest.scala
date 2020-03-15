package flawlessly

import flawless.SuiteClass

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
import com.softwaremill.diffx.cats._
import cats.Parallel
import cats.mtl.MonadState
import cats.mtl.FunctorTell
import cats.mtl.instances.all._
import cats.Alternative
import cats.Monad
import cats.data.`package`.ReaderWriterStateT

//Sync, because Bracket for WriterT isn't explicitly written
final class InterpreterReportingTest[F[_]: Sync] extends SuiteClass[F] {
  type WC[A] = ReaderWriterStateT[F, Unit, Chain[Reporter.Event[Int]], Int, A]

  //The instance shall not be used for parallelism! It's pretty much just a marker
  implicit val parallelState: Parallel[WC] = Parallel.identity

  import Sync.catsReaderWriteStateTSync

  val reporter: Reporter.Aux[WC, Int] = {
    def make[
      M[_]: MonadState[*[_], Int]: Monad: FunctorTell[*[_], S[Reporter.Event[Int]]],
      S[_]: Alternative
    ]: Reporter.Aux[M, Int] = new Reporter[M] {
      type Identifier = Int

      val ident: M[Identifier] = MonadState[M, Int].get <* MonadState[M, Int].modify(_ + 1)

      def publish(event: Reporter.Event[Identifier]): M[Unit] =
        FunctorTell[M, S[Reporter.Event[Int]]].tell(event.pure[S])
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

  def simpleEvents(id: Int): List[Reporter.Event[Int]] = List(
    SuiteStarted("suite 1", id),
    TestStarted("test 1"),
    TestFinished("test 1"),
    TestStarted("test 2"),
    TestFinished("test 2"),
    SuiteFinished("suite 1", id, succeeded = true)
  )

  def simpleResource(suite: Suite[WC]): Suite[WC] = Suite.resource(suite.pure[Resource[WC, *]])

  def ensureReported[G[_]: Foldable](suite: Suite[WC])(expectedWritten: G[Reporter.Event[Int]]): F[Assertion] =
    interpreter.interpret(reporter)(suite).written.runA((), 0).map(_.toList).map(ensureEqual(_, expectedWritten.toList))

  //todo: these would be good property tests
  val runSuite: Suite[F] = suite(getClass.getSimpleName) {
    tests(
      test("single suite") {
        ensureReported(simpleSuite)(simpleEvents(0))
      },
      test("sequence of suites") {
        ensureReported {
          Suite.sequential(simpleSuite, simpleSuite)
        } {
          List(ReplaceSuiteWith(0, NonEmptyList.of(1, 2))) ++
            simpleEvents(1) ++
            simpleEvents(2)
        }
      },
      test("nested sequence of suites - left side") {
        ensureReported {
          Suite.sequential(Suite.sequential(simpleSuite), simpleSuite, simpleSuite)
        } {
          List(ReplaceSuiteWith(0, NonEmptyList.of(1, 2, 3))) ++
            simpleEvents(1) ++
            simpleEvents(2) ++
            simpleEvents(3)
        }
      },
      test("nested sequence of suites - right side") {
        ensureReported {
          Suite.sequential(simpleSuite, simpleSuite, Suite.sequential(simpleSuite))
        } {
          List(ReplaceSuiteWith(0, NonEmptyList.of(1, 2, 3))) ++
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
          List(ReplaceSuiteWith(0, NonEmptyList.of(1, 2, 3))) ++
            simpleEvents(1) ++
            simpleEvents(2) ++
            List(ReplaceSuiteWith(3, NonEmptyList.of(4, 5, 6, 7))) ++
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
          List(ReplaceSuiteWith(0, NonEmptyList.of(1, 2))) ++
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
            ReplaceSuiteWith(0, NonEmptyList.of(1, 2, 3)).pure[List],
            simpleEvents(1),
            simpleEvents(2),
            ReplaceSuiteWith(3, NonEmptyList.of(4, 5)).pure[List],
            simpleEvents(4),
            simpleEvents(5)
          ).flatten
        }
      }
    )
  }
}
