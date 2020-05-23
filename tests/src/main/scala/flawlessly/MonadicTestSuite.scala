package flawlessly

import flawless._
import flawless.syntax._
import cats.effect.Sync
import cats.Applicative
import flawless.eval.Interpreter
import flawless.eval.Reporter
import cats.implicits._

object MonadicTestSuite {

  def runSuite[F[_]: Sync]: Suite[F] = suite("MonadicTestSuite") {
    val interpreter = Interpreter.defaultInterpreter[F]

    tests(
      test("monadic tests fail when no assertions are given") {
        Sync[F].suspend {
          val underTest = suite("demo")(testMonadic[F]("demo")(_ => Applicative[F].unit))

          val result = interpreter.interpret(Reporter.noop[F]).apply(underTest).map(flawless.eval.summarize)

          //this has lots of trouble without the function composition
          //todo: investigate before merge
          (ensureM(result) _)
            .compose {
              select(_.stats.test.failed) _
            }
            .compose(equalTo[Int])
            .apply(1)
        }
      }
    )
  }
}
