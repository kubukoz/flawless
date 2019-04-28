package flawless.syntax

import flawless.Assertions
import flawless.SuiteResult

import cats.Functor
import cats.implicits._
import cats.data.NonEmptyList
import flawless.TestResult
import cats.kernel.Semigroup
import cats.kernel.Eq
import cats.Show
import flawless.Assertion
import flawless.AssertionFailure
import flawless.stats.Location
import cats.NonEmptyParallel
import cats.Id
import cats.FlatMap
import cats.effect.Clock
import java.util.concurrent.TimeUnit
import flawless.SuiteMetadata

trait TestCompiler[F[_]] {
  def compile[A](fa: F[SuiteResult]): F[SuiteResult]
}

object TestCompiler {
  implicit val idTestCompiler: TestCompiler[Id] = new TestCompiler[Id] {
    def compile[A](fa: SuiteResult): SuiteResult = fa
  }

  implicit def timedTestCompiler[F[_]: Clock: FlatMap]: TestCompiler[F] = new TestCompiler[F] {
    val now = Clock[F].monotonic(TimeUnit.MILLISECONDS)

    def compile[A](fa: F[SuiteResult]): F[SuiteResult] = for {
      before <- now
      result <- fa
      after <- now
    } yield result.copy(meta = Some(SuiteMetadata(before, after)))
  }
}

class Dsl[F[_]: Functor] extends ExtraParallelSyntax {

  def test(name: String)(
    ftest: F[Assertions]
  )(implicit compiler: TestCompiler[F]): F[SuiteResult] = compiler.compile {
    ftest.map { result =>
      SuiteResult(None, NonEmptyList.one(TestResult(name, result)))
    }
  }

  /*
   * If you like to write each test in its own line, this is a handy helper that'll make it possible.
   * Instead of combining tests with the semigroup, pass them to this function
   * as you would to e.g. the List(...) constructor.
   */
  def tests(
    first: F[SuiteResult],
    others: F[SuiteResult]*
  )(implicit S: Semigroup[F[SuiteResult]]): F[SuiteResult] =
    NonEmptyList(first, others.toList).reduce

  //anyval maybe?
  implicit class ShouldBeSyntax[A](actual: A) {

    def shouldBe(expected: A)(
      implicit eq: Eq[A],
      show: Show[A],
      file: sourcecode.File,
      line: sourcecode.Line
    ): Assertions = {
      val assertion =
        if (eq.eqv(actual, expected))
          Assertion.Successful
        else
          Assertion.Failed(
            AssertionFailure(
              show"""Reason: $actual (actual) wasn't equal to $expected (expected)""",
              Location(file.value, line.value)
            )
          )

      Assertions(NonEmptyList.one(assertion))
    }
  }
}

trait ExtraParallelSyntax {
  implicit class ParallelCombineOps[F[_], A](fa: F[A]) {

    def |&|[M[_]](
      another: F[A]
    )(implicit S: Semigroup[A], P: NonEmptyParallel[F, M]): F[A] = {
      P.sequential(P.apply.map2(P.parallel(fa), P.parallel(another))(S.combine))
    }
  }
}
