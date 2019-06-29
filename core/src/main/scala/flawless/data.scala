package flawless

import cats.Functor
import cats.NonEmptyParallel
import cats.NonEmptyTraverse
import cats.Parallel
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import cats.implicits._
import cats.kernel.Semigroup
import flawless.data.low.Tests
import flawless.data.low.Tests.Both
import flawless.data.low.Tests.LiftResource
import flawless.data.low.Tests.Run
import flawless.data.low.Tests.Sequence
import flawless.fixpoint.HFix
import flawless.stats.Location

final class TTest[A] private[flawless] (private[flawless] val tree: HFix[Tests, A]) {
  def interpret: IO[A] = HFix.hCata(tree)(Tests.algebras.interpret)
  def visit(v: IO[SuiteResult] => IO[SuiteResult]): TTest[A] = HFix.hCata(tree)(Tests.algebras.visitRun(v))
  def debug: String = HFix.hCata(tree)(Tests.algebras.show)
}

object TTest {
  def liftIO(result: IO[SuiteResult]): TTest[SuiteResult] = new TTest(HFix[Tests, SuiteResult](Run(result)))
  def liftResource[A, B](tests: Resource[IO, A])(f: A => TTest[B]): TTest[B] = new TTest(HFix(LiftResource(tests, f.map(_.tree))))

  def parSequence[S[_]: NonEmptyTraverse, A](suites: S[TTest[A]])(implicit nep: NonEmptyParallel[IO, IO.Par]): TTest[S[A]] =
    new TTest(HFix(Sequence[Tests.HFixed, S, A](suites.map(_.tree), Parallel.parNonEmptySequence(_), Functor[S])))

  def sequence[S[_]: NonEmptyTraverse, A](suites: S[TTest[A]]): TTest[S[A]] =
    new TTest(HFix(Sequence[Tests.HFixed, S, A](suites.map(_.tree), _.nonEmptySequence, Functor[S])))

  implicit def semigroup[F[_], A](implicit F: Semigroup[A]): Semigroup[TTest[A]] =
    (a, b) => new TTest(HFix[Tests, A](Both(a.tree, b.tree, F)))
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
  def runSuite: TTest[SuiteResult]
}
