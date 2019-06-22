package flawless

import cats.implicits._
import cats.effect.IO
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import flawless.stats.Location
import cats.Parallel
import cats.NonEmptyTraverse
import cats.Foldable
import cats.effect.ContextShift
import cats.Applicative
import cats.Functor

sealed trait Tests[A] {
  def interpret(implicit contextShift: ContextShift[IO]): IO[A]
  final def liftA[F[_]: Applicative]: Tests[F[A]] = this.map(_.pure[F])
}

object Tests {
  def liftIO[F[_]: Foldable](result: IO[F[SuiteResult]]): Tests[F[SuiteResult]] = liftIOA(result)

  def parSequence[F[_]: NonEmptyTraverse](suites: F[Tests[SuiteResult]]): Tests[F[SuiteResult]] =
    new Sequence(suites, true) {}

  def sequence[F[_]: NonEmptyTraverse](suites: F[Tests[SuiteResult]]): Tests[F[SuiteResult]] =
    new Sequence(suites, false) {}

  implicit val testsFunctor: Functor[Tests] = new Functor[Tests] {
    def map[A, B](fa: Tests[A])(f: A => B): Tests[B] = new Tests.Map(fa, f) {}
  }

  private def liftIOA[A]: IO[A] => Tests[A] = new Run(_) {}

  sealed abstract case class Run[A](iotest: IO[A]) extends Tests[A] {
    def interpret(implicit contextShift: ContextShift[IO]): IO[A] = iotest
  }

  sealed abstract class Map[A, B](tests: Tests[A], f: A => B) extends Tests[B] {
    def interpret(implicit contextShift: ContextShift[IO]): IO[B] = tests.interpret.map(f)
  }

  sealed abstract case class Sequence[F[_], A](tests: F[Tests[A]], parallel: Boolean)(implicit F: NonEmptyTraverse[F]) extends Tests[F[A]] {

    def interpret(implicit contextShift: ContextShift[IO]): IO[F[A]] =
      if (parallel) Parallel.parNonEmptyTraverse(tests)(_.interpret)
      else tests.nonEmptyTraverse(_.interpret)
  }

  sealed abstract case class Pure[A](value: A) extends Tests[A] {
    def interpret(implicit contextShift: ContextShift[IO]): IO[A] = /* value.pure[IO] */ ???
  }

  sealed abstract class Both[A: Semigroup](left: Tests[A], right: Tests[A]) extends Tests[A] {
    def interpret(implicit contextShift: ContextShift[IO]): IO[A] = (left.interpret |+| right.interpret)
  }

  implicit def semigroup[F[_], A](implicit F: Semigroup[A]): Semigroup[Tests[A]] = new Both(_, _) {}
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
