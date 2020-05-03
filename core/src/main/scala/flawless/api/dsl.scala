package flawless.api

import flawless.data._
import cats.data._
import cats.implicits._
import com.softwaremill.diffx.Diff
import cats.Show
import cats.Eval
import flawless.PredicateT
import cats.kernel.Eq
import flawless.Predicate
import cats.Functor
import flawless.{Suite => _, _}
import cats.effect.kernel.Ref
import cats.effect.MonadThrow

trait AllDsl {

  def suite[F[_]](name: String)(tests: NonEmptyList[Test[F]]): Suite[F] = Suite.one(name, tests)

  def tests[F[_]](firstTest: NonEmptyList[Test[F]], rest: NonEmptyList[Test[F]]*): NonEmptyList[Test[F]] =
    NonEmptyList(firstTest, rest.toList).reduce

  // Effectful test.
  // Note that all side effects need to be suspended in the context of F. Relying on the laziness of the parameter is discouraged.
  def test[F[_]](name: String)(assertions: => F[Assertion]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Eval(Eval.later(assertions))))

  /** Provides access to assertions in a monadic fashion.
    * If no assertions are added, the test completes with a single failed assertion (making it impossible to have a green test without assertions).
    */
  def testMonadic[F[_]: Ref.Make: MonadThrow](name: String)(assertions: Assert[F] => F[Unit]): NonEmptyList[Test[F]] =
    test[F](name) {
      Ref[F]
        .of(none[Assertion])
        .flatTap { ref =>
          val assert = Assert.refInstance(ref)

          // I'm not super excited about this, but this might be the only way to keep the previous assertions.
          // Unless we use a runtime-injected error handler (kind of like an algebraic effect)
          // so that we can continue here but delegate the error handling to there.
          assertions(assert).recoverWith { case TODO() =>
            assert(Assertion.pending)
          }
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

  def ??? : Nothing = throw TODO()
}

//todo: move
final case class TODO() extends Exception {

  override def toString(): String =
    """An uncaught TODO exception has been thrown.
      |
      |This may mean that `???` was used from outside a test or suite execution, for example in a suite's enclosing object or class.
      |If it was used in an effect like IO, or within a test/suite, please report an issue on https://github.com/kubukoz/flawless/issues.""".stripMargin
}
