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

sealed trait Suites[F[_]] extends Product with Serializable {
  def interpret(implicit interpreter: Interpreter[F]): F[Suites[Id]] = interpreter.interpret(this)
}

object Suites {
  def one[F[_]](suite: Suite[F]): Suites[F] = One(suite)

  def parallel[F[_]: NonEmptyParallel](first: Suites[F], rest: Suites[F]*): Suites[F] =
    parSequence(NonEmptyList(first, rest.toList))

  def parSequence[F[_]: NonEmptyParallel](suitesSequence: NonEmptyList[Suites[F]]): Suites[F] =
    Sequence[F](suitesSequence, Traversal.parallel)

  def sequential[F[_]: Apply](first: Suites[F], rest: Suites[F]*): Suites[F] =
    sequence(NonEmptyList(first, rest.toList))

  def sequence[F[_]: Apply](suitesSequence: NonEmptyList[Suites[F]]): Suites[F] =
    Sequence[F](suitesSequence, Traversal.sequential)

  def resource[F[_]: Bracket[*[_], Throwable]](suitesInResource: Resource[F, Suites[F]]): Suites[F] =
    RResource(suitesInResource, Bracket[F, Throwable])

  def suspend[F[_]](suites: F[Suites[F]]): Suites[F] = Suspend(suites)

  final case class Sequence[F[_]](suites: NonEmptyList[Suites[F]], traversal: Traversal[F]) extends Suites[F]
  final case class One[F[_]](suite: Suite[F]) extends Suites[F]
  final case class Suspend[F[_]](suites: F[Suites[F]]) extends Suites[F]
  final case class RResource[F[_]](resuites: Resource[F, Suites[F]], bracket: Bracket[F, Throwable]) extends Suites[F]

  def flatten(suites: Suites[Id]): NonEmptyList[Suite[Id]] = suites match {
    case Sequence(suites, _) => suites.flatMap(flatten)
    case One(suite)          => suite.pure[NonEmptyList]
    case Suspend(suites)     => flatten(suites)
    case RResource(_, _)     => throw new AssertionError("Impossible")
  }
}

/**
  * An abstraction on methods of combining two effects - parallel or sequential
  */
sealed trait Traversal[F[_]] extends Product with Serializable {

  final def traverse[S[_]: NonEmptyTraverse, A, B](as: S[A])(f: A => F[B]): F[S[B]] = this match {
    case Traversal.Sequential(a) =>
      implicit val apply = a
      as.nonEmptyTraverse(f)
    case Traversal.Parallel(nep) =>
      implicit val parallel = nep
      Parallel.parNonEmptyTraverse(as)(f)
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

final case class Suite[+F[_]](name: String, tests: NonEmptyList[Test[F]]) {
  def toSuites[F2[a] >: F[a]]: Suites[F2] = Suites.one(this)
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
