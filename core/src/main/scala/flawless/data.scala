package flawless

import cats.implicits._
import cats.effect.IO
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import flawless.stats.Location
import cats.Parallel
import cats.~>
import cats.Apply
import cats.arrow.FunctionK
import cats.NonEmptyParallel
import cats.NonEmptyTraverse
import cats.Foldable
import cats.effect.ContextShift

sealed trait Tests[A] {
  def interpret0(implicit contextShift: ContextShift[IO]): IO[A] = interpret(FunctionK.id)

  def interpret(fk: IO ~> IO)(implicit contextShift: ContextShift[IO]): IO[A] = this match {
    case Tests.Run(iotest)             => fk(iotest)
    case Tests.ParMap2(left, right, f) => (left.interpret(fk), right.interpret(fk)).parMapN(f)
    case Tests.FlatMap(fa, f)          => fk(fa.interpret(fk)).flatMap(a => fk(f(a).interpret(fk)))
    case Tests.Pure(a)                 => IO.pure(a)
  }
}

object Tests {
  def liftIO[F[_]: Foldable](result: IO[F[SuiteResult]]): Tests[F[SuiteResult]] = liftIOA(result)

  def parallel[F[_]: NonEmptyTraverse](suites: F[Tests[SuiteResult]]): Tests[F[SuiteResult]] =
    Parallel.parNonEmptySequence(suites)

  def sequential[F[_]: NonEmptyTraverse](suites: F[Tests[SuiteResult]]): Tests[F[SuiteResult]] =
    suites.nonEmptySequence

  private def liftIOA[A]: IO[A] => Tests[A] = new Run(_) {}
  private def pureA[A]: A => Tests[A] = new Pure(_) {}

  sealed abstract case class ParMap2[A, B, C](left: Tests[A], right: Tests[B], f: (A, B) => C) extends Tests[C]
  sealed abstract case class FlatMap[A, B](fa: Tests[A], f: A => Tests[B]) extends Tests[B]
  sealed abstract case class Run[A](iotest: IO[A]) extends Tests[A]
  sealed abstract case class Pure[A](value: A) extends Tests[A]

  implicit val flatMap: cats.FlatMap[Tests] = new cats.FlatMap[Tests] {
    def flatMap[A, B](fa: Tests[A])(f: A => Tests[B]): Tests[B] = new FlatMap(fa, f) {}
    def map[A, B](fa: Tests[A])(f: A => B): Tests[B] = new FlatMap[A, B](fa, a => pureA(f(a))) {}

    def tailRecM[A, B](a: A)(f: A => Tests[Either[A, B]]): Tests[B] = f(a).flatMap {
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => Tests.pureA(b)
    }
  }

  implicit val catsParalleForTests: NonEmptyParallel[Tests, ParallelTests] = new NonEmptyParallel[Tests, ParallelTests] {
    val apply: Apply[ParallelTests] = ParallelTests.parallelApply
    val flatMap: cats.FlatMap[Tests] = Tests.flatMap
    val parallel: Tests ~> ParallelTests = λ[Tests ~> ParallelTests](ParallelTests(_))
    val sequential: ParallelTests ~> Tests = λ[ParallelTests ~> Tests](_.sequential)
  }

  implicit def semigroup[A: Semigroup]: Semigroup[Tests[A]] = Apply.semigroup
}

final case class ParallelTests[A](sequential: Tests[A]) extends AnyVal

object ParallelTests {
  implicit val parallelApply: Apply[ParallelTests] = new Apply[ParallelTests] {
    def map[A, B](fa: ParallelTests[A])(f: A => B): ParallelTests[B] = ParallelTests(fa.sequential.map(f))

    def ap[A, B](ff: ParallelTests[A => B])(fa: ParallelTests[A]): ParallelTests[B] =
      ParallelTests(new Tests.ParMap2[A => B, A, B](ff.sequential, fa.sequential, (f, a) => f(a)) {})
  }
}

final case class AssertionFailure(text: String, location: Location)

final case class Assertions(value: NonEmptyList[Assertion])

object Assertions {
  implicit val semigroup: Semigroup[Assertions] = (a, b) => Assertions(a.value |+| b.value)
}

sealed trait Assertion extends Product with Serializable {
  def isSuccessful: Boolean = fold(true, _ => false)
  def isFailed: Boolean = !isSuccessful

  def fold[A](successful: => A, failed: AssertionFailure => A): A = this match {
    case Assertion.Successful       => successful
    case Assertion.Failed(failures) => failed(failures)
  }
}

object Assertion {
  case object Successful extends Assertion
  final case class Failed(failure: AssertionFailure) extends Assertion
}

final case class TestResult(name: String, assertions: Assertions)

final case class SuiteResult(results: NonEmptyList[TestResult]) extends AnyVal

object SuiteResult {
  implicit val semigroup: Semigroup[SuiteResult] = (a, b) => SuiteResult(a.results |+| b.results)
}

trait Suite { self =>
  def runSuite: Tests[SuiteResult]
}
