package flawless

import cats.effect.IO
import cats.effect.Resource
import cats.implicits._
import cats.kernel.Semigroup
import cats.Functor
import cats.Id
import cats.NonEmptyParallel
import cats.NonEmptyTraverse
import cats.Parallel
import cats.Show
import cats.data.NonEmptyList
import flawless.data.low.TestAlg
import flawless.data.low.TestAlg.LiftResource
import flawless.data.low.TestAlg.Merge
import flawless.data.low.TestAlg.Run
import flawless.data.low.TestAlg.Pure
import flawless.fixpoint.HFix
import flawless.stats.Location

final class Tests[A] private[flawless] (private[flawless] val tree: HFix[TestAlg, A]) {
  def interpret: IO[A] = HFix.hCata(tree)(TestAlg.algebras.interpret)
  def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[A] = HFix.hCata(tree)(TestAlg.algebras.visitRun(v))

  def debugRun: IO[String] = HFix.hCata(tree)(TestAlg.algebras.show)
}

object Tests {
  def pure(result: SuiteResult): Tests[SuiteResult] = new Tests(HFix[TestAlg, SuiteResult](Pure(result)))
  def liftIO(result: IO[SuiteResult]): Tests[SuiteResult] = new Tests(HFix[TestAlg, SuiteResult](Run(result)))

  def resource[A](resource: Resource[IO, A]): TestsResource[A] = new TestsResource(resource)

  final class TestsResource[A] private[Tests] (private val resource: Resource[IO, A]) extends AnyVal {
    def use[B](f: A => Tests[B]): Tests[B] = new Tests(HFix(LiftResource(resource, f.map(_.tree))))
  }

  def parSequence[S[_]: NonEmptyTraverse, A](suites: S[Tests[A]])(implicit nep: NonEmptyParallel[IO, IO.Par]): Tests[S[A]] =
    new Tests(HFix(Merge[TestAlg.HFixed, S, S, A](suites.map(_.tree), Parallel.parNonEmptySequence(_), Functor[S])))

  def sequence[S[_]: NonEmptyTraverse, A](suites: S[Tests[A]]): Tests[S[A]] =
    new Tests(HFix(Merge[TestAlg.HFixed, S, S, A](suites.map(_.tree), _.nonEmptySequence, Functor[S])))

  //todo test
  implicit def semigroup[F[_], A](implicit A: Semigroup[A]): Semigroup[Tests[A]] =
    (a, b) => {
      val alg: TestAlg[TestAlg.HFixed, A] =
        Merge[TestAlg.HFixed, NonEmptyList, Id, A](NonEmptyList.of(a.tree, b.tree), _.reduce, Functor[NonEmptyList])

      new Tests(
        HFix[TestAlg, A](alg)
      )
    }

  //todo test
  //todo consider removing (replace with some method of lifting A to an effect like NonEmptyList)
  implicit val functor: Functor[Tests] = new Functor[Tests] {
    override def map[A, B](fa: Tests[A])(f: A => B): Tests[B] = new Tests(HFix(TestAlg.Map(fa.tree, f)))
  }
}

final case class AssertionFailure(text: String, location: Location)

final case class Assertions(value: NonEmptyList[Assertion]) extends AnyVal

object Assertions {
  def apply(first: Assertion, rest: Assertion*): Assertions = Assertions(NonEmptyList.of(first, rest: _*))

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
  implicit val show: Show[SuiteResult] = Show.fromToString
}

trait Suite { self =>
  def runSuite: Tests[SuiteResult]
}
