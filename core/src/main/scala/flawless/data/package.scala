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

/**
  * A value of this type is a non-empty sequence of test suites. Each suite must have a name.
  * Suites can be composed to run sequentially or in parallel to each other. Effects and resources can also be lifted to a Suite.
  */
sealed trait Suite[+F[_]] extends Product with Serializable {

  /**
    * Rename each suite in this value using the given function.
    * todo: this should be an aspect
    *  */
  def renameEach[F2[a] >: F[a]: FlatMap](modName: String => F2[String]): Suite[F2] =
    Suite.renameEach(modName).apply(this)

  /**
    * Combine the two sets of suites sequentially.
    */
  def zip[F2[a] >: F[a]: Apply](another: Suite[F2]): Suite[F2] = Suite.sequential(this, another)

  /**
    * Combine the two sets of suites in parallel.
    * */
  def parZip[F2[a] >: F[a]: NonEmptyParallel](another: Suite[F2]): Suite[F2] = Suite.parallel(this, another)

  /**
    * Alias for [[zip]].
    */
  def |+|[F2[a] >: F[a]: Apply](another: Suite[F2]): Suite[F2] = this zip another

  /**
    * Alias for [[parZip]].
    */
  def |&|[F2[a] >: F[a]: NonEmptyParallel](another: Suite[F2]): Suite[F2] = this parZip another

  // Duplicate this suite n times in parallel. For the sequential version, use semigroup syntax.
  def parCombineN[F2[a] >: F[a]: NonEmptyParallel](n: Int): Suite[F2] =
    Suite.parSequence[F2](NonEmptyList.one(this).combineN(n))

  /**
    * Widen the effect type of this suite.
    */
  def widenF[F2[a] >: F[a]]: Suite[F2] = this

  private[flawless] def interpret[F2[a] >: F[a]](implicit interpreter: Interpreter[F2]): F2[Suite[NoEffect]] =
    interpreter.interpret(this)
}

object Suite {
  def one[F[_]](name: String, tests: NonEmptyList[Test[F]]): Suite[F] = algebra.One(name, tests)

  def parallel[F[_]: NonEmptyParallel](first: Suite[F], rest: Suite[F]*): Suite[F] =
    parSequence(NonEmptyList(first, rest.toList))

  def parSequence[F[_]: NonEmptyParallel](suitesSequence: NonEmptyList[Suite[F]]): Suite[F] =
    combineWith(suitesSequence)(Traversal.parallel)

  def sequential[F[_]: Apply](first: Suite[F], rest: Suite[F]*): Suite[F] =
    sequence(NonEmptyList(first, rest.toList))

  def sequence[F[_]: Apply](suitesSequence: NonEmptyList[Suite[F]]): Suite[F] =
    combineWith(suitesSequence)(Traversal.sequential)

  def combineWith[F[_]](suites: NonEmptyList[Suite[F]])(traversal: Traversal[F]): Suite[F] =
    algebra.Sequence(suites, traversal)

  def resource[F[_]: Bracket[*[_], Throwable]](suitesInResource: Resource[F, Suite[F]]): Suite[F] =
    algebra.RResource(suitesInResource, Bracket[F, Throwable])

  def suspend[F[_]](suites: F[Suite[F]]): Suite[F] = algebra.Suspend(suites)

  /**
    * Semigroup instance that combines suites sequentially.
    */
  implicit def suiteSemigroup[F[_]: Apply]: Semigroup[Suite[F]] = new Semigroup[Suite[F]] {
    def combine(x: Suite[F], y: Suite[F]): Suite[F] = x.zip(y)
  }

  def renameEach[F[_]: FlatMap](modName: String => F[String]): Suite[F] => Suite[F] = {
    //todo what about stack safety in the case of tests in Id?
    def go(self: Suite[F]): F[Suite[F]] = self match {
      case o: algebra.One[f] =>
        //typing newName manually is needed due to GADT limitation
        Functor[f].map(modName(o.name))(algebra.One[f](_: String, o.tests))

      case s: algebra.Sequence[f]  => s.traversal.traverse(s.suites)(go)
      case r: algebra.RResource[f] => r.resuite.evalMap(go)(r.bracket)
      case s: algebra.Suspend[f]   => s.suite.flatMap(go)
    }

    (go _).andThen(Suite.suspend)
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
