package flawlessly

import flawless.SuiteClass

import flawless.data.Suite
import flawless.dsl._
import flawless._
import cats.implicits._
import cats.data.Writer
import cats.data.Chain
import flawless.eval.Reporter
import flawless.eval.Interpreter
import cats.Show
import cats.data.StateT

object ReporterTest extends SuiteClass[NoEffect] {
  type WC[A] = StateT[Writer[Chain[Reporter.Event], ?], Reporter.SuiteHistory, A]

  val reporter: Reporter[WC] = a => StateT.liftF(Writer.tell(Chain.one(a)))
  val interpreter: Interpreter[WC] = Interpreter.defaultInterpreter[WC]

  implicit val showEvent: Show[Reporter.Event] = Show.fromToString

  val runSuite: Suite[NoEffect] = suite("ReporterTest") {
    tests(
      pureTest("suite with two tests") {

        val testedSuite = suite[WC]("suite 1") {
          tests(
            pureTest("test 1")(ensureEqual(1, 1)),
            pureTest("test 2")(ensureEqual(1, 1))
          )
        }

        import Reporter.Event._

        val expectedOut = List(
          SuiteStarted("suite 1"),
          TestStarted("test 1"),
          TestFinished("test 1"),
          TestStarted("test 2"),
          TestFinished("test 2"),
          SuiteFinished("suite 1")
        )

        ensureEqual(
          interpreter.interpret(reporter)(testedSuite).runA(Reporter.SuiteHistory(0)).written.toList,
          expectedOut
        )
      }
    )
  }
}
