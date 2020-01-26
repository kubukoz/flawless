package flawless.data

import flawless.Interpreter
import cats.data.NonEmptyList
import cats.implicits._
import cats.Applicative
import cats.Id
import cats.NonEmptyParallel
import cats.Apply
import cats.Parallel
import cats.NonEmptyTraverse
import cats.effect.concurrent.Ref
import cats.data.Chain
import cats.Foldable
import cats.effect.Resource
import cats.effect.Bracket
import flawless.data.Assertion.Failed
import flawless.data.Assertion.Successful
import cats.Functor
import cats.FlatMap

import Suite.algebra
import flawless.dsl.NoEffect
import cats.kernel.Semigroup

sealed trait Assertion extends Product with Serializable {

  def isSuccessful: Boolean = this match {
    case Failed(_)  => false
    case Successful => true
  }
}

object Assertion {
  case object Successful extends Assertion
  final case class Failed(message: String) extends Assertion
}

/**
  * An abstraction on methods of combining two effects - parallel or sequential
  */
sealed trait Traversal[F[_]] extends Product with Serializable {

  final def traverse[S[_]: NonEmptyTraverse, A, B](as: S[A])(f: A => F[B]): F[S[B]] = this match {
    case Traversal.Sequential(a) => as.nonEmptyTraverse(f)(a)
    case Traversal.Parallel(nep) => Parallel.parNonEmptyTraverse(as)(f)(NonEmptyTraverse[S], nep)
  }

  final def sequence[S[_]: NonEmptyTraverse, A](as: S[F[A]]): F[S[A]] = traverse(as)(identity)
}

object Traversal {

  final private case class Parallel[F[_]](nep: NonEmptyParallel[F]) extends Traversal[F]
  final private case class Sequential[F[_]](apply: Apply[F]) extends Traversal[F]

  // (potentially specialized) implementation of Sequential for Id,
  // which means identity in case of `sequence` and `map` in case of `traverse`.
  val identity: Traversal[Id] = sequential

  def sequential[F[_]: Apply]: Traversal[F] = Sequential(Apply[F])
  def parallel[F[_]: NonEmptyParallel]: Traversal[F] = Parallel(NonEmptyParallel[F])
}

sealed trait Suite[+F[_]] extends Product with Serializable {

  //This will probably be an aspect later
  def renameEach[F2[a] >: F[a]: FlatMap](modName: String => F2[String]): Suite[F2] = {

    //todo what about stack safety in the case of effectless tests?
    def go(self: Suite[F2]): F2[Suite[F2]] = self match {
      case o: algebra.One[f] =>
        //typing newName manually is needed due to GADT limitation
        Functor[f].map(modName(o.name))(algebra.One[f](_: String, o.tests))

      case s: algebra.Sequence[f]  => s.traversal.traverse(s.suites)(go)
      case r: algebra.RResource[f] => r.resuite.evalMap(go)(r.bracket)
      case s: algebra.Suspend[f]   => s.suite.flatMap(go)
    }

    Suite.suspend(go(this))
  }

  private[flawless] def interpret[F2[a] >: F[a]](implicit interpreter: Interpreter[F2]): F2[Suite[NoEffect]] =
    interpreter.interpret(this)

  //I swear, this variance thing is going to end this library
  def zip[F2[a] >: F[a]: Apply](another: Suite[F2]): Suite[F2] = this |+| another
  def parZip[F2[a] >: F[a]: NonEmptyParallel](another: Suite[F2]): Suite[F2] = this |&| another
  def |+|[F2[a] >: F[a]: Apply](another: Suite[F2]): Suite[F2] = Semigroup[Suite[F2]].combine(this, another)
  def |&|[F2[a] >: F[a]: NonEmptyParallel](another: Suite[F2]): Suite[F2] = Suite.parallel(this, another)

  // Duplicate this suite n times. For the sequential version, use semigroup syntax.
  def parCombineN[F2[a] >: F[a]: NonEmptyParallel](n: Int): Suite[F2] =
    Suite.parSequence[F2](NonEmptyList.one(this).combineN(n))
}

object Suite {
  def one[F[_]](name: String, tests: NonEmptyList[Test[F]]): Suite[F] = algebra.One(name, tests)

  def parallel[F[_]: NonEmptyParallel](first: Suite[F], rest: Suite[F]*): Suite[F] =
    parSequence(NonEmptyList(first, rest.toList))

  def parSequence[F[_]: NonEmptyParallel](suitesSequence: NonEmptyList[Suite[F]]): Suite[F] =
    algebra.Sequence[F](suitesSequence, Traversal.parallel)

  def sequential[F[_]: Apply](first: Suite[F], rest: Suite[F]*): Suite[F] =
    sequence(NonEmptyList(first, rest.toList))

  def sequence[F[_]: Apply](suitesSequence: NonEmptyList[Suite[F]]): Suite[F] =
    algebra.Sequence[F](suitesSequence, Traversal.sequential)

  def resource[F[_]: Bracket[*[_], Throwable]](suitesInResource: Resource[F, Suite[F]]): Suite[F] =
    algebra.RResource(suitesInResource, Bracket[F, Throwable])

  def suspend[F[_]](suites: F[Suite[F]]): Suite[F] = algebra.Suspend(suites)

  implicit def suiteSemigroup[F[_]: Apply]: Semigroup[Suite[F]] = new Semigroup[Suite[F]] {
    def combine(x: Suite[F], y: Suite[F]): Suite[F] = Suite.sequential(x, y)
  }

  private[flawless] object algebra {
    final case class One[F[_]](name: String, tests: NonEmptyList[Test[F]]) extends Suite[F]
    final case class Sequence[F[_]](suites: NonEmptyList[Suite[F]], traversal: Traversal[F]) extends Suite[F]
    final case class Suspend[F[_]](suite: F[Suite[F]]) extends Suite[F]
    final case class RResource[F[_]](resuite: Resource[F, Suite[F]], bracket: Bracket[F, Throwable]) extends Suite[F]
  }

  private[flawless] val flatten: Suite[Id] => NonEmptyList[algebra.One[Id]] = {
    case algebra.Sequence(suites, _) => suites.flatMap(flatten)
    case o @ algebra.One(_, _)       => o.pure[NonEmptyList]
    case algebra.Suspend(suites)     => flatten(suites)
    case algebra.RResource(_, _)     => throw new AssertionError("Impossible! Id doesn't form resources.")
  }
}

//todo rename result to run
final case class Test[+F[_]](name: String, result: TestRun[F])

sealed trait TestRun[+F[_]] extends Product with Serializable {

  def assertions[F2[a] >: F[a]](implicit applicative: Applicative[F2]): F2[NonEmptyList[Assertion]] = this match {
    case TestRun.Eval(effect) => effect
    case TestRun.Pure(result) => result.pure[F2]
    case TestRun.Lazy(result) => result.value.pure[F2]
  }
}

object TestRun {
  final case class Eval[F[_]](effect: F[NonEmptyList[Assertion]]) extends TestRun[F]
  final case class Pure(result: NonEmptyList[Assertion]) extends TestRun[Nothing]
  final case class Lazy(result: cats.Eval[NonEmptyList[Assertion]]) extends TestRun[Nothing]
}

trait Assertions[F[_]] {
  def add(assertion: Assertion): F[Unit]
  def addAll[S[_]: Foldable](assertions: S[Assertion]): F[Unit]
}

object Assertions {

  def refInstance[F[_]: Applicative](ref: Ref[F, Chain[Assertion]]): Assertions[F] =
    new Assertions[F] {
      def add(assertion: Assertion): F[Unit] = ref.update(_.append(assertion))

      def addAll[S[_]: Foldable](assertions: S[Assertion]): F[Unit] =
        ref.update(_.concat(Chain.fromSeq(assertions.toList)))
    }
}
