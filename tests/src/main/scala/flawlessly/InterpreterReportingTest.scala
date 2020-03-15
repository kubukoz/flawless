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
import cats.data.WriterT
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.Bracket
import cats.Foldable
import flawless.data.Assertion
import cats.data.NonEmptyList
import cats.data.StateT
import com.softwaremill.diffx.cats._
import cats.Parallel

//Sync, because Bracket for WriterT isn't explicitly written
final class InterpreterReportingTest[F[_]: Sync] extends SuiteClass[F] {
  type WCC[A] = WriterT[F, Chain[Reporter.Event[Int]], A]
  type WC[A] = StateT[WCC, Int, A]

  //for Bracket instances
  import Sync.catsStateTSync
  import Sync.catsWriterTSync

  //The instance shall not be used for parallelism!
  implicit val parallelState: Parallel[WC] = Parallel.identity

  val reporter: Reporter[WC] { type Identifier = Int } = new Reporter[WC] {
    type Identifier = Int

    val ident: WC[Identifier] = StateT { i =>
      (i + 1, i).pure[WCC]
    }
    def publish(event: Reporter.Event[Identifier]): WC[Unit] = StateT.liftF(WriterT.tell(event.pure[Chain]))
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
    interpreter.interpret(reporter)(suite).runA(0).written.map(_.toList).map(ensureEqual(_, expectedWritten.toList))

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
