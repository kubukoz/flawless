package flawless.data

import cats.data.NonEmptyChain
import cats.implicits._
import cats.Applicative
import cats.Id
import cats.NonEmptyParallel
import cats.Apply
import cats.Parallel
import cats.NonEmptyTraverse
import cats.effect.concurrent.Ref
import cats.effect.Resource
import cats.effect.Bracket
import cats.Functor
import cats.FlatMap

import cats.kernel.Semigroup
import cats.kernel.Monoid
import flawless.data.Assertion.Result
import flawless.data.Assertion.One
import flawless.data.Assertion.All
import scala.annotation.tailrec
import cats.data.Chain
import cats.data.NonEmptyList
import cats.kernel.Eq
import cats.mtl.MonadState
import cats.Monad

sealed trait Assertion extends Product with Serializable {

  def results: NonEmptyChain[Result] = {
    // Manual recursion instead of trampolining, to save some space on lambdas
    @tailrec
    def go(remaining: List[Assertion], results: Chain[Result]): Chain[Result] = remaining match {
      case One(result) :: t     => go(t, results.append(result))
      case All(assertions) :: t => go(assertions.toList ::: t, results)
      case Nil                  => results
    }

    // This is really safe, okay?
    // Nil could only ever be returned if no assertions were provided, and we start with `this`, so there's always one.
    // If this ever throws - there's a bug in flawless.
    NonEmptyChain.fromChainUnsafe(go(List(this), Chain.nil))
  }
}

object Assertion {
  val successful: Assertion = one(Result.Successful)
  def failed(message: String): Assertion = one(Result.Failed(message))

  def thrown(e: Throwable): Assertion =
    failed(
      show"An unexpected ${e.getClass().getName()} was thrown: ${e
        .getMessage()}\n${e.getStackTrace().map(_.toString).map("  at " + _).mkString("\n")}"
    )

  def one(result: Result): Assertion = One(result)

  def all(assertions: NonEmptyChain[Assertion]): Assertion = assertions.reduceLeft { (a, b) =>
    All {
      (a, b) match {
        case (All(x1), All(x2)) => (x1 <+> x2)
        case (All(x1), _)       => x1.append(b)
        case (_, All(y2))       => y2.prepend(a)
        case _                  => NonEmptyChain(a, b)
      }
    }
  }

  sealed trait Result extends Product with Serializable {

    def isSuccessful: Boolean = this match {
      case Result.Failed(_)  => false
      case Result.Successful => true
    }
  }

  object Result {
    case object Successful extends Result
    final case class Failed(message: String) extends Result
  }

  final case class One(result: Result) extends Assertion
  final case class All(assertions: NonEmptyChain[Assertion]) extends Assertion

  implicit val assertionMonoid: Monoid[Assertion] = new Monoid[Assertion] {
    def combine(x: Assertion, y: Assertion): Assertion = all(NonEmptyChain(x, y))
    val empty: Assertion = successful
  }
}

/**
  * An abstraction on methods of combining two effects - parallel or sequential
  */
sealed trait Traversal[F[_]] extends Product with Serializable {

  def kind: Traversal.Kind = this match {
    case Traversal.Parallel(_)   => Traversal.Kind.Parallel
    case Traversal.Sequential(_) => Traversal.Kind.Sequential
  }

  final def traverse[S[_]: NonEmptyTraverse, A, B](as: S[A])(f: A => F[B]): F[S[B]] = this match {
    case Traversal.Sequential(a) => as.nonEmptyTraverse(f)(a)
    case Traversal.Parallel(nep) => Parallel.parNonEmptyTraverse(as)(f)(NonEmptyTraverse[S], nep)
  }

  final def sequence[S[_]: NonEmptyTraverse, A](as: S[F[A]]): F[S[A]] = traverse(as)(identity)
}

object Traversal {

  final private case class Parallel[F[_]](nep: NonEmptyParallel[F]) extends Traversal[F]
  final private case class Sequential[F[_]](apply: Apply[F]) extends Traversal[F]

  sealed trait Kind extends Product with Serializable

  object Kind {
    case object Parallel extends Kind
    case object Sequential extends Kind

    implicit val eq: Eq[Kind] = Eq.fromUniversalEquals
  }

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

  def combineWith[F[_]](suites: NonEmptyList[Suite[F]])(traversal: Traversal[F]): Suite[F] = {
    val flattenedSameTraversal: NonEmptyList[Suite[F]] = suites.flatMap {
      case seq: algebra.Sequence[f] if seq.traversal.kind === traversal.kind => seq.suites
      case other: Suite[F]                                                   => NonEmptyList.one(other)
    }

    algebra.Sequence(flattenedSameTraversal, traversal)
  }

  def resource[F[_]: Bracket[*[_], Throwable]](suitesInResource: Resource[F, Suite[F]]): Suite[F] =
    algebra.RResource(suitesInResource, Bracket[F, Throwable])

  def suspend[F[_]](suites: F[Suite[F]]): Suite[F] = algebra.Suspend(suites)

  def thrown(e: Throwable): Suite.algebra.One[flawless.NoEffect] =
    algebra.One("failed", NonEmptyList.one(Test.thrown(e)))

  /**
    * Semigroup instance that combines suites sequentially.
    */
  implicit def suiteSemigroup[F[_]: Apply]: Semigroup[Suite[F]] = _ zip _

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

object Test {
  def thrown(e: Throwable): Test[flawless.NoEffect] = Test("failed", TestRun.Pure(Assertion.thrown(e)))
}

sealed trait TestRun[+F[_]] extends Product with Serializable {

  def assertions[F2[a] >: F[a]](implicit applicative: Applicative[F2]): F2[Assertion] = this match {
    case TestRun.Eval(effect) => effect
    case TestRun.Pure(result) => result.pure[F2]
    case TestRun.Lazy(result) => result.value.pure[F2]
  }
}

object TestRun {
  final case class Eval[F[_]](effect: F[Assertion]) extends TestRun[F]
  final case class Pure(result: Assertion) extends TestRun[Nothing]
  final case class Lazy(result: cats.Eval[Assertion]) extends TestRun[Nothing]
}

trait Assert[F[_]] {
  def apply(assertion: Assertion): F[Unit]
}

object Assert {

  def refInstance[F[_]: Monad](ref: Ref[F, Option[Assertion]]): Assert[F] = {
    import com.olegpy.meow.effects._

    ref.runState { implicit S =>
      monadStateInstance[F]
    }
  }

  def monadStateInstance[F[_]: MonadState[*[_], Option[Assertion]]]: Assert[F] =
    assertion => MonadState[F, Option[Assertion]].modify(_ |+| assertion.some)
}
