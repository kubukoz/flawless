package flawlessly

import flawless.SuiteClass

import flawless.data.Suite
import flawless.dsl._
import flawless._
import cats.implicits._
import cats.effect.ConsoleOut
import cats.data.Writer
import cats.data.Chain
import flawless.eval.Reporter
import flawless.eval.Interpreter
import cats.Show

object ReporterTest extends SuiteClass[NoEffect] {
  type WC[A] = Writer[Chain[String], A]

  implicit val cout: ConsoleOut[WC] = new ConsoleOut[WC] {
    def putStr[A: Show](a: A): WC[Unit] = Writer.tell(Chain(a.show))
    def putStrLn[A: Show](a: A): WC[Unit] = Writer.tell(Chain(show"$a\n"))
  }

  implicit val reporter: Reporter[WC] = Reporter.consoleInstance[WC]

  val interpreter: Interpreter[WC] = Interpreter.defaultInterpreter[WC]

  val runSuite: Suite[NoEffect] = suite("ReporterTest") {
    tests(
      pureTest("suite with two tests") {

        val testedSuite = suite[WC]("suite 1") {
          tests(
            pureTest("test 1")(ensureEqual(1, 1)),
            pureTest("test 2")(ensureEqual(1, 1))
          )
        }

        val expectedOut =
          show"""Starting suite: suite 1
                |  Starting test: test 1
                |  Finished test: test 1
                |  Starting test: test 2
                |  Finished test: test 2
                |Finished suite: suite 1""".stripMargin.linesIterator.toList.map(_ + "\n")

        ensureEqual(interpreter.interpret(testedSuite).written.toList, expectedOut)
      }
    )
  }
}
