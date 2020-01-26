package flawless

import flawless.data._
import cats.data._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import com.softwaremill.diffx.Diff
import com.softwaremill.diffx.DiffResult
import cats.Show
import cats.Eval
import cats.Order

object dsl {
  // Shamelessly ripped off from fs2's Pure type - this is pure genius.
  type NoEffect[A] <: Nothing

  // This name is bad (Predicate implies A => Boolean). Come up with a better name.
  // Possibly worth newtyping.
  // This idea is heavily inspired by ZIO Test.
  type Predicate[-A] = A => Assertion

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
    ensure(actual, predicates.all.equalTo(expected))
  def assertion(cond: Boolean, ifFalse: String): Assertion = ensure(cond, predicates.all.isTrue(ifFalse))
}

object predicates {

  object all {
    import dsl.Predicate

    def greaterThan[A: Order: Show](another: A): Predicate[A] =
      a => if (a > another) Assertion.successful else Assertion.failed(show"$a was not greater than $another")

    def equalTo[T: Diff: Show](another: T): Predicate[T] = {
      implicit val showDiff: Show[DiffResult] = _.show

      a =>
        Diff[T].apply(a, another) match {
          case diff if diff.isIdentical => Assertion.successful
          case diff                     => Assertion.failed(show"$a (actual) was not equal to $another (expected). Diff: $diff")
        }
    }

    val successful: Predicate[Any] = _ => Assertion.successful
    def failed(message: String): Predicate[Any] = _ => Assertion.failed(message)

    def isTrue(ifFalse: String): Predicate[Boolean] = {
      case true  => Assertion.successful
      case false => Assertion.failed(ifFalse)
    }
  }
}
