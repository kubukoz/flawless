package flawlessly

import flawless.syntax._
import flawless.data.Suite
import flawless.eval.Interpreter
import cats.Defer
import flawless.eval.Reporter
import cats.implicits._
import cats.data.NonEmptyList
import flawless.eval.Output
import flawless.eval.Output.Problem
import flawless.Predicate
import flawless.PredicateT
import flawless.data.Assertion
import flawless.eval.RunStats
import flawless.eval.RunStats.Stat
import cats.effect.MonadThrow
import cats.effect.kernel.Ref

object InterpreterCatchingTest {

  def apply[F[_]: MonadThrow: Defer: Ref.Make]: Suite[F] = {
    val interpreter: Interpreter[F] = Interpreter.defaultInterpreter[F]

    val summarized: Predicate[Output] => PredicateT[F, Suite[F]] =
      p =>
        select[Suite[F]] {
          interpreter.interpret(Reporter.noop)(_).map(flawless.eval.summarize(_))
        }(p.liftM[F])

    val summarizedSuites: Predicate[NonEmptyList[Output.Suite]] => PredicateT[F, Suite[F]] =
      summarized.compose(select[Output](_.suites))

    val wholeSuitePending = summarizedSuites(
      equalToEq(
        NonEmptyList.of(
          Output.Suite(
            "demo",
            NonEmptyList.of(
              Output.Test(
                "demo-test-1",
                problems = Problem.Pending.leftNel
              )
            )
          )
        )
      )
    )

    val testCatchingTest = test("Pending tests are shown as pending") {
      val suiteUnderTest: Suite[F] = suite("demo")(
        test("demo-test-1")(Defer[F].defer(???))
      )

      ensure(
        suiteUnderTest,
        wholeSuitePending
      )
    }

    val testCatchingByNameTest = test("Pends are caught without user suspension") {
      val suiteUnderTest = suite[F]("demo") {
        test("demo-test-1")(???)
      }

      ensure(
        suiteUnderTest,
        wholeSuitePending
      )
    }

    val monadicTestsCatchingTest = test("Monadic tests return all previous assertions if a pending one is found") {
      val suiteUnderTest = suite("demo") {
        testMonadic[F]("monadic demo") { assert =>
          assert(Assertion.failed("failure 1")) *>
            assert(Assertion.successful) >>
            ???
        }
      }

      val expectedOutput = Output(
        RunStats(
          Stat(0, 0, 1),
          Stat(0, 0, 1),
          Stat(1, 1, 1)
        ),
        NonEmptyList.of(
          Output.Suite(
            "demo",
            NonEmptyList.of(
              Output.Test("monadic demo", NonEmptyList.of(Problem.Failed("failure 1"), Problem.Pending).asLeft)
            )
          )
        )
      )

      ensure(
        suiteUnderTest,
        summarized.compose(equalToEq[Output])(expectedOutput)
      )
    }

    suite("InterpreterCatchingTest") {
      tests(testCatchingTest, testCatchingByNameTest, monadicTestsCatchingTest)
    }
  }

}
