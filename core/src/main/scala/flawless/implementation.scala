package flawless

import cats.implicits._
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import flawless.stats.Location
import scala.concurrent.duration.FiniteDuration

case class AssertionFailure(text: String, location: Location)

case class Assertions(value: NonEmptyList[Assertion])

object Assertions {
  implicit val semigroup: Semigroup[Assertions] = (a, b) =>
    Assertions(a.value |+| b.value)
}

sealed trait Assertion extends Product with Serializable {
  def isSuccessful: Boolean = fold(true, _ => false)
  def isFailed: Boolean     = !isSuccessful

  def fold[A](successful: => A, failed: AssertionFailure => A): A = this match {
    case Assertion.Successful       => successful
    case Assertion.Failed(failures) => failed(failures)
  }
}

object Assertion {
  case object Successful                       extends Assertion
  case class Failed(failure: AssertionFailure) extends Assertion
}

case class TestResult(name: String, assertions: Assertions)

case class SuiteMetadata(timeStart: Long, timeEnd: Long)

object SuiteMetadata {
  implicit val semigroup: Semigroup[SuiteMetadata] = (a, b) =>
    SuiteMetadata(a.timeStart min b.timeStart, a.timeEnd max b.timeEnd)
}

case class SuiteResult(meta: Option[SuiteMetadata], results: NonEmptyList[TestResult])

object SuiteResult {
  implicit val semigroup: Semigroup[SuiteResult] = (a, b) =>
    SuiteResult(a.meta |+| b.meta, a.results |+| b.results)
}

trait Suite { self =>
  def runSuite: IOTest[SuiteResult]
}
