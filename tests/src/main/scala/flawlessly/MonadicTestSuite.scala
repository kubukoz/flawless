package flawlessly

import flawless._
import flawless.syntax._
import cats.Applicative
import flawless.data.Assertion
import cats.data.NonEmptyList
import flawless.data.Test
import cats.data.Writer
import cats.effect.kernel.Ref
import cats.effect.MonadThrow

object MonadicTestSuite {

  def apply[F[_]: Ref.Make: MonadThrow]: Suite[F] = suite("MonadicTestSuite") {

    def assertions(assertionPredicate: Predicate[Assertion]): PredicateT[F, Test[F]] =
      select[Test[F]](_.result.assertions[F])(assertionPredicate.liftM[F])

    val failedDueToAssertions: PredicateT[F, Test[F]] =
      assertions(equalToEq(Assertion.failed("No assertions were made!")))

    tests(
      test("monadic tests fail when no assertions are given") {
        //todo: tests shouldn't return lists anymore... I know, free monoid :(
        val NonEmptyList(theTest, _) = testMonadic[F]("demo")(_ => Applicative[F].unit)

        ensure(theTest, failedDueToAssertions)
      },
      test("monadic (state) tests fail when no assertions are given") {
        val NonEmptyList(theTest, _) = pureTestMonadic("demo")(_ => Writer.tell(None))

        ensure(theTest, failedDueToAssertions)
      }
    )
  }

}
