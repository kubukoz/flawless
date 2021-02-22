package flawless.api

import flawless.data._
import cats.data._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import com.softwaremill.diffx.Diff
import cats.Show
import cats.Eval
import flawless.PredicateT
import cats.kernel.Eq
import flawless.Predicate
import cats.Functor
import flawless.{Suite => _, _}

trait AllDsl {

  def suite[F[_]](name: String)(tests: NonEmptyList[Test[F]]): Suite[F] = Suite.one(name, tests)

  def tests[F[_]](firstTest: NonEmptyList[Test[F]], rest: NonEmptyList[Test[F]]*): NonEmptyList[Test[F]] =
    NonEmptyList(firstTest, rest.toList).reduce

  def test[F[_]](name: String)(assertions: F[Assertion]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Eval(assertions)))

  /** Provides access to assertions in a monadic fashion.
    * If no assertions are added, the test completes with a single failed assertion (making it impossible to have a green test without assertions).
    */
  def testMonadic[F[_]: Sync](name: String)(assertions: Assert[F] => F[Unit]): NonEmptyList[Test[F]] =
    test[F](name) {
      Ref[F]
        .of(none[Assertion])
        .flatTap { ref =>
          assertions(Assert.refInstance(ref))
        }
        .flatMap(_.get)
        .map(_.getOrElse(Assertion.failed("No assertions were made!")))
    }

  type AssertionState[A] = Writer[Option[Assertion], A]

  def pureTestMonadic(name: String)(f: Assert[AssertionState] => AssertionState[Unit]): NonEmptyList[Test[Nothing]] = {
    val assertInstance = Assert.functorTellInstance[AssertionState]

    pureTest(name) {
      f(assertInstance).written.getOrElse(Assertion.failed("No assertions were made!"))
    }
  }

  def pureTest(name: String)(assertions: Assertion): NonEmptyList[Test[Nothing]] =
    NonEmptyList.one(Test(name, TestRun.Pure(assertions)))

  def lazyTest(name: String)(assertions: => Assertion): NonEmptyList[Test[Nothing]] =
    NonEmptyList.one(Test(name, TestRun.Lazy(Eval.later(assertions))))

  object ensure {
    //todo inconsistent with ensureM.apply
    def apply[F[_], A](value: A, predicate: PredicateT[F, A]): F[Assertion] = predicate(value)
    def fun[F[_], A](value: A)(predicate: A => F[Assertion]): F[Assertion] = ensure(value, PredicateT(predicate))
  }

  object ensureM {
    def apply[F[_]: Functor, A](value: F[A])(predicate: Predicate[A]): F[Assertion] = ensure(value, predicate.liftM[F])

    def fun[F[_]: Functor, A](value: F[A])(predicate: A => Assertion): F[Assertion] =
      apply(value)(PredicateT[cats.Id, A](predicate))
  }

  //todo naming
  def ensureEqualEq[A: Eq: Show](actual: A, expected: A): Assertion =
    ensure(actual, flawless.predicates.equalToEq(expected))

  def ensureEqual[A: Diff: Show](actual: A, expected: A): Assertion =
    ensure(actual, flawless.predicates.equalTo(expected))

  def assertion(cond: Boolean, ifFalse: String): Assertion = ensure(cond, flawless.predicates.isTrue(ifFalse))
}
