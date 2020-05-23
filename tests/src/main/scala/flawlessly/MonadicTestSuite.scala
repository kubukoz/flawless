package flawlessly

import flawless._
import flawless.syntax._
import cats.effect.Sync
import cats.Applicative
import flawless.data.Assertion
import cats.data.NonEmptyList

object MonadicTestSuite {

  def runSuite[F[_]: Sync]: Suite[F] = suite("MonadicTestSuite") {

    tests(
      test("monadic tests fail when no assertions are given") {
        Sync[F].suspend {
          //todo: tests shouldn't return lists anymore... I know, free monoid :(
          val NonEmptyList(theTest, _) = testMonadic[F]("demo")(_ => Applicative[F].unit)

          ensureM(theTest.result.assertions)(
            equalToEq(Assertion.failed("No assertions were made!"))
          )
        }
      }
    )
  }
}
