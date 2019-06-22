package flawless

import cats.implicits._
import cats.effect.IO
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import flawless.stats.Location
import cats.Monad
import cats.StackSafeMonad
import cats.Applicative
import cats.Parallel
import cats.~>
import flawless.Tests.Run
import flawless.Tests.ParMap2
import flawless.Tests.FlatMap
import flawless.Tests.Pure

sealed trait Tests[A] {
  def visit(f: IO ~> IO): Tests[A] = Tests.Visit(this, f)

  def interpret(fk: IO ~> IO)(implicit parallelIO: Parallel[IO, IO.Par]): IO[A] = this match {
    case Pure(a)                 => a.pure[IO]
    case Run(iotest)             => fk(iotest)
    case ParMap2(left, right, f) => (fk(left.interpret(fk)), fk(right.interpret(fk))).parMapN(f)
    case FlatMap(fa, f)          => fk(fa.interpret(fk)).flatMap(a => fk(f(a).interpret(fk)))
    case Tests.Visit(fa, fk)     => fa.interpret(fk)
  }
}

object Tests {
  case class Pure[A](a: A) extends Tests[A]
  case class ParMap2[A, B, C](left: Tests[A], right: Tests[B], f: (A, B) => C) extends Tests[C]
  case class FlatMap[A, B](fa: Tests[A], f: A => Tests[B]) extends Tests[B]
  case class Run[A](iotest: IO[A]) extends Tests[A]
  case class Visit[A](tests: Tests[A], fk: IO ~> IO) extends Tests[A]

  implicit val monad: Monad[Tests] = new StackSafeMonad[Tests] {
    def pure[A](x: A): Tests[A] = Tests.Pure(x)
    def flatMap[A, B](fa: Tests[A])(f: A => Tests[B]): Tests[B] = FlatMap(fa, f)
  }

  implicit val catsParalleForTests: Parallel[Tests, ParallelTests] = new Parallel[Tests, ParallelTests] {
    val applicative: Applicative[ParallelTests] = ParallelTests.parallelApplicative
    val monad: Monad[Tests] = Tests.monad
    val parallel: Tests ~> ParallelTests = λ[Tests ~> ParallelTests](ParallelTests(_))
    val sequential: ParallelTests ~> Tests = λ[ParallelTests ~> Tests](_.sequential)
  }
}

sealed abstract case class ParallelTests[A](val sequential: Tests[A])

object ParallelTests {
  def apply[A](tests: Tests[A]): ParallelTests[A] = new ParallelTests[A](tests) {}

  implicit val parallelApplicative: Applicative[ParallelTests] = new Applicative[ParallelTests] {
    def pure[A](x: A): ParallelTests[A] = ParallelTests(Tests.Pure(x))

    def ap[A, B](ff: ParallelTests[A => B])(fa: ParallelTests[A]): ParallelTests[B] =
      ParallelTests(Tests.ParMap2[A => B, A, B](ff.sequential, fa.sequential, (f, a) => f(a)))
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
  def runSuite: IO[SuiteResult]
}
