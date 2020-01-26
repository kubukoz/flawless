package flawless.api

import flawless.data._
import cats.data._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import com.softwaremill.diffx.Diff
import cats.Show
import cats.Eval
import flawless.Predicate

trait AllDsl {

  def suite[F[_]](name: String)(tests: NonEmptyList[Test[F]]): Suite[F] = Suite.one(name, tests)

  def tests[F[_]](firstTest: NonEmptyList[Test[F]], rest: NonEmptyList[Test[F]]*): NonEmptyList[Test[F]] =
    NonEmptyList(firstTest, rest.toList).reduce

  def test[F[_]](name: String)(assertions: F[Assertion]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Eval(assertions)))

  /**
    * Provides access to assertions in a monadic fashion.
    * If no assertions are added, the test completes with a single successful assertion.
    */
  def testMonadic[F[_]: Sync](name: String)(assertions: Assert[F] => F[Unit]): NonEmptyList[Test[F]] =
    test[F](name) {
      Ref[F]
        .of(Assertion.successful)
        .flatTap { ref =>
          assertions(Assert.refInstance(ref))
        }
        .flatMap(_.get)
    }

  def pureTest(name: String)(assertions: Assertion): NonEmptyList[Test[Nothing]] =
    NonEmptyList.one(Test(name, TestRun.Pure(assertions)))

  def lazyTest(name: String)(assertions: => Assertion): NonEmptyList[Test[Nothing]] =
    NonEmptyList.one(Test(name, TestRun.Lazy(Eval.later(assertions))))

  def ensure[A](value: A, predicate: Predicate[A]): Assertion = predicate(value)

  def ensureEqual[A: Diff: Show](actual: A, expected: A): Assertion =
    ensure(actual, flawless.predicates.equalTo(expected))

  def assertion(cond: Boolean, ifFalse: String): Assertion = ensure(cond, flawless.predicates.isTrue(ifFalse))
}
