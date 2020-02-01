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

//Sync, because Bracket for WriterT isn't explicitly written
final class InterpreterReportingTest[F[_]: Sync] extends SuiteClass[F] {
  type WC[A] = WriterT[F, Chain[Reporter.Event], A]

  implicit val bracketWriter: Bracket[WC, Throwable] = Sync.catsWriterTSync

  val reporter: Reporter[WC] = a => WriterT.tell(a.pure[Chain])
  val interpreter: Interpreter[WC] = Interpreter.defaultInterpreter[WC]

  implicit val showEvent: Show[Reporter.Event] = Show.fromToString

  val simpleSuite = suite[WC]("suite 1") {
    tests(
      pureTest("test 1")(ensureEqual(1, 1)),
      pureTest("test 2")(ensureEqual(1, 1))
    )
  }

  import Reporter.Event._

  val simpleEvents = List(
    SuiteStarted("suite 1"),
    TestStarted("test 1"),
    TestFinished("test 1"),
    TestStarted("test 2"),
    TestFinished("test 2"),
    SuiteFinished("suite 1")
  )

  def simpleResource(suite: Suite[WC]): Suite[WC] = Suite.resource(suite.pure[Resource[WC, *]])

  def ensureReported[G[_]: Foldable](suite: Suite[WC])(expectedWritten: G[Reporter.Event]): F[Assertion] =
    interpreter.interpret(reporter)(suite).written.map(_.toList).map(ensureEqual(_, expectedWritten.toList))

  //todo: these would be good property tests
  val runSuite: Suite[F] = suite(getClass.getSimpleName) {
    tests(
      test("single suite") {
        ensureReported(simpleSuite)(simpleEvents)
      },
      test("sequence of suites") {
        ensureReported {
          Suite.sequential(simpleSuite, simpleSuite)
        } {
          List(ExtraSuitesReported(1)) ++
            simpleEvents ++
            simpleEvents
        }
      },
      test("resource doesn't allocate extra suites") {
        ensureReported(simpleResource(simpleSuite))(simpleEvents)
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
            ExtraSuitesReported(2).pure[List],
            simpleEvents,
            simpleEvents,
            ExtraSuitesReported(1).pure[List],
            simpleEvents,
            simpleEvents
          ).flatten
        }
      }
    )
  }
}
