package flawless.tests
import flawless.SuiteClass

import flawless.data.Suite
import flawless.dsl._
import cats.implicits._
import flawless.Reporter
import cats.effect.ConsoleOut
import cats.data.Writer
import cats.data.Chain
import flawless.Interpreter
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
        val testedSuite = suite("suite 1") {
          tests(
            test[WC]("test 1")(ensureEqual(1, 1).pure[WC]),
            test[WC]("test 2")(ensureEqual(1, 1).pure[WC])
          )
        }.toSuites

        val test1Result = "Pure(NonEmptyList(Successful))"
        val test2Result = "Pure(NonEmptyList(Successful))"
        val suiteResult = show"NonEmptyList(Test(test 1,$test1Result), Test(test 2,$test2Result))"

        val expectedOut =
          show"""Starting suite: suite 1
                |  Starting test: test 1
                |  Finished test: test 1, result: $test1Result
                |  Starting test: test 2
                |  Finished test: test 2, result: $test2Result
                |Finished suite: suite 1, result: $suiteResult""".stripMargin.linesIterator.toList.map(_ + "\n")

        ensureEqual(interpreter.interpret(testedSuite).written.toList, expectedOut)
      }
    )
  }
}
