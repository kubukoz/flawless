package flawless

import cats.implicits._
import cats.effect.IO
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import flawless.stats.Location
import cats.Parallel
import cats.NonEmptyTraverse
import cats.Applicative
import cats.Functor
import cats.effect.Resource
import cats.NonEmptyParallel

sealed trait Tests[A] {
  def interpret: IO[A]
  def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[A]
  final def liftA[F[_]: Applicative]: Tests[F[A]] = this.map(_.pure[F])

}

object Tests {
  import structure._

  def liftIO(result: IO[SuiteResult]): Tests[SuiteResult] = new Run(result) {}
  def liftResource[A, B](tests: Resource[IO, A])(f: A => Tests[B]): Tests[B] = new LiftResource(tests, f) {}

  def parSequence[F[_]: NonEmptyTraverse](
    suites: F[Tests[SuiteResult]]
  )(implicit nep: NonEmptyParallel[IO, IO.Par]
  ): Tests[F[SuiteResult]] =
    new Sequence[F, SuiteResult](suites, Parallel.parNonEmptySequence(_)) {}

  def sequence[F[_]: NonEmptyTraverse](suites: F[Tests[SuiteResult]]): Tests[F[SuiteResult]] =
    new Sequence[F, SuiteResult](suites, _.nonEmptySequence) {}

  implicit val testsFunctor: Functor[Tests] = new Functor[Tests] {
    def map[A, B](fa: Tests[A])(f: A => B): Tests[B] = new structure.Map(fa, f) {}
  }

  private[flawless] object structure {
    sealed abstract case class Run(iotest: IO[SuiteResult]) extends Tests[SuiteResult] {
      def interpret: IO[SuiteResult] = iotest
      def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[SuiteResult] = new Run(v(iotest)) {}
    }

    sealed abstract case class Map[A, B](tests: Tests[A], f: A => B) extends Tests[B] {
      def interpret: IO[B] = tests.interpret.map(f)
      def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[B] = new Map(tests.visit(v), f) {}
    }

    sealed abstract case class LiftResource[A, B](resource: Resource[IO, A], f: A => Tests[B]) extends Tests[B] {
      def interpret: IO[B] = resource.use(f(_).interpret)
      def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[B] = new LiftResource[A, B](resource, f(_).visit(v)) {}
    }

    sealed abstract case class Sequence[F[_], A](
      tests: F[Tests[A]],
      merge: (F[IO[A]] => IO[F[A]])
    )(implicit F: Functor[F])
      extends Tests[F[A]] {

      def interpret: IO[F[A]] =
        merge(tests.map(_.interpret))

      def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[F[A]] = new Sequence(tests.map(_.visit(v)), merge) {}
    }

    sealed abstract case class Both[A: Semigroup](left: Tests[A], right: Tests[A]) extends Tests[A] {
      def interpret: IO[A] = (left.interpret |+| right.interpret)
      def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[A] = new Both(left.visit(v), right.visit(v)) {}
    }
  }

  implicit def semigroup[F[_], A](implicit F: Semigroup[A]): Semigroup[Tests[A]] = new Both(_, _) {}
}

final case class AssertionFailure(text: String, location: Location)

final case class Assertions(value: NonEmptyList[Assertion])

object Assertions {
  implicit val semigroup: Semigroup[Assertions] = (a, b) => Assertions(a.value |+| b.value)
}

sealed trait Assertion extends Product with Serializable {
  def isFailed: Boolean = fold(false, _ => true)
  def isSuccessful: Boolean = !isFailed

  def fold[A](successful: => A, failed: AssertionFailure => A): A = this match {
    case Assertion.Successful       => successful
    case Assertion.Failed(failures) => failed(failures)
  }
}

object Assertion {
  case object Successful extends Assertion
  final case class Failed(failure: AssertionFailure) extends Assertion
}

final case class TestResult(name: String, assertions: Assertions) {
  def isFailed: Boolean = assertions.value.exists(_.isFailed)
  def isSuccessful: Boolean = !isFailed
}

final case class SuiteResult(results: NonEmptyList[TestResult]) extends AnyVal {
  def isFailed: Boolean = results.exists(_.isFailed)
  def isSuccessful: Boolean = !isFailed
}

object SuiteResult {
  implicit val semigroup: Semigroup[SuiteResult] = (a, b) => SuiteResult(a.results |+| b.results)
}

trait Suite { self =>
  def runSuite: Tests[SuiteResult]
}
