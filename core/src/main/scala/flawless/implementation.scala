package flawless

import cats.Show
import cats.kernel.Eq
import cats.effect.IO
import cats.implicits._
import cats.data.NonEmptyList
import cats.data.Kleisli
import cats.kernel.Semigroup
import cats.kernel.Monoid

case class TestRun(
  only: List[String],
  except: List[String]
)

case class AssertionFailure(text: String) extends AnyVal

//glorified Validated
sealed trait Outcome extends Product with Serializable

object Outcome {
  case object Successful                                      extends Outcome
  case class Failed(failures: NonEmptyList[AssertionFailure]) extends Outcome

  object Failed {
    def one(failure: AssertionFailure): Outcome = Failed(NonEmptyList.one(failure))
  }

  implicit val monoid: Monoid[Outcome] = new Monoid[Outcome] {
    val empty: Outcome = Successful

    def combine(x: Outcome, y: Outcome): Outcome = (x, y) match {
      case (Failed(e1), Failed(e2)) => Failed(e1 |+| e2)
      case (Successful, _)          => y
      case (_, Successful)          => x
    }
  }
}

case class TestResult(name: String, outcome: Outcome)

case class SpecResult(results: NonEmptyList[TestResult])

object SpecResult {
  implicit val semigroup: Semigroup[SpecResult] = (a, b) =>
    SpecResult(a.results |+| b.results)
}

trait Spec {
  def runSpec: IOTest[SpecResult]
}

//todo: figure out how to always run pure specs in parallel...
trait PureSpec extends Spec {
  def runPure: PureTest[SpecResult]

  final override val runSpec: IOTest[SpecResult] = Kleisli { config =>
    IO(runPure.run(config))
  }
}

trait Dsl[F[_]] {

  //assertions in a test are always sequential
  //unless you explicitly make them parallel in effectful tests
  def test(name: String)(
    ftest: F[Outcome]
  ): Kleisli[F, TestRun, SpecResult] = ???

  //anyval
  implicit class ShouldBeSyntax[A](actual: A) {

    def shouldBe(expected: A)(implicit eq: Eq[A], show: Show[A]): Outcome =
      if (eq.eqv(actual, expected))
        Outcome.Successful
      else
        Outcome.Failed.one(
          AssertionFailure(
            show"""$actual (actual) wasn't equal to $expected (expected)"""
          )
        )
  }
}
