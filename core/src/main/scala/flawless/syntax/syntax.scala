package flawless

import flawless.data._
import cats.data._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.data.Chain
import cats.implicits._
import com.softwaremill.diffx.Diff
import com.softwaremill.diffx.DiffResult
import cats.Show
import cats.Eval
import cats.Order
import sourcecode.Enclosing
import simulacrum.typeclass

@typeclass
trait AsLabel[A] {
  def asLabel(a: A): String
}

object AsLabel {
  implicit val stringAsLabel: AsLabel[String] = identity(_)
  implicit val enclosingAsLabel: AsLabel[Enclosing] = _.value
}

object dsl {
  // Shamelessly ripped off from fs2's Pure type - this is pure genius.
  type NoEffect[A] <: Nothing

  // This name is bad (Predicate implies A => Boolean). Come up with a better name.
  // Possibly worth newtyping.
  // This idea is heavily inspired by ZIO Test.
  type Predicate[-A] = A => Assertion

  def autoLabel(implicit enc: Enclosing): Enclosing = enc

  final class PartiallyAppliedSuite[F[_]](private val dummy: Boolean = false) extends AnyVal {

    def apply[Label: AsLabel](label: Label)(tests: NonEmptyList[Test[F]]): Suite[F] =
      new Suite(AsLabel[Label].asLabel(label), tests)
  }

  def suite[F[_]]: PartiallyAppliedSuite[F] = new PartiallyAppliedSuite[F]()

  def tests[F[_]](firstTest: NonEmptyList[Test[F]], rest: NonEmptyList[Test[F]]*): NonEmptyList[Test[F]] =
    NonEmptyList(firstTest, rest.toList).reduce

  def test[F[_]](name: String)(assertions: F[NonEmptyList[Assertion]]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Eval(assertions)))

  /**
    * Provides access to assertions in a monadic fashion.
    * If no assertions are added, the test completes with a single successful assertion.
    */
  def testMonadic[F[_]: Sync](name: String)(assertions: Assertions[F] => F[Unit]): NonEmptyList[Test[F]] =
    test[F](name) {
      Ref[F]
        .of(Chain.empty[Assertion])
        .flatMap { ref =>
          assertions(Assertions.refInstance(ref)) *> ref.get
        }
        .map(_.toList.toNel.getOrElse(NonEmptyList.one(Assertion.Successful)))
    }

  def pureTest(name: String)(assertions: NonEmptyList[Assertion]): NonEmptyList[Test[Nothing]] =
    NonEmptyList.one(Test(name, TestRun.Pure(assertions)))

  def lazyTest(name: String)(assertions: => NonEmptyList[Assertion]): NonEmptyList[Test[Nothing]] =
    NonEmptyList.one(Test(name, TestRun.Lazy(Eval.later(assertions))))

  def ensure[A](value: A, predicate: Predicate[A]): NonEmptyList[Assertion] = NonEmptyList.one(predicate(value))

  def ensureEqual[A: Diff: Show](actual: A, expected: A): NonEmptyList[Assertion] =
    ensure(actual, predicates.all.equalTo(expected))
  def assertion(cond: Boolean, ifFalse: String): NonEmptyList[Assertion] = ensure(cond, predicates.all.isTrue(ifFalse))
}

object predicates {

  object all {
    import dsl.Predicate

    def greaterThan[A: Order: Show](another: A): Predicate[A] =
      a => if (a > another) Assertion.Successful else Assertion.Failed(show"$a was not greater than $another")

    def equalTo[T: Diff: Show](another: T): Predicate[T] = {
      implicit val showDiff: Show[DiffResult] = _.show

      a =>
        Diff[T].apply(a, another) match {
          case diff if diff.isIdentical => Assertion.Successful
          case diff                     => Assertion.Failed(show"$a (actual) was not equal to $another (expected). Diff: $diff")
        }
    }

    val successful: Predicate[Any] = _ => Assertion.Successful
    def failed(message: String): Predicate[Any] = _ => Assertion.Failed(message)

    def isTrue(ifFalse: String): Predicate[Boolean] = {
      case true  => Assertion.Successful
      case false => Assertion.Failed(ifFalse)
    }
  }
}
