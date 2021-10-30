package flawless.api

import flawless.data._
import cats.implicits._
import com.softwaremill.diffx.Diff
import com.softwaremill.diffx.DiffResult
import cats.Show
import cats.Order
import flawless.Predicate
import flawless.PredicateT
import cats.kernel.Eq

trait AllPredicates {

  def greaterThan[A: Order: Show](another: A): Predicate[A] = Predicate { a =>
    if (a > another) Assertion.successful else Assertion.failed(show"$a was not greater than $another")
  }

  def equalTo[T: Diff: Show](expected: T): Predicate[T] = Predicate {
    implicit val showDiff: Show[DiffResult] = _.show

    actual =>
      Diff[T].apply(actual, expected) match {
        case diff if diff.isIdentical => Assertion.successful
        case diff                     =>
          Assertion
            .failed(
              show"$actual (actual) was not equal to $expected (expected). Diff: ${Console.RESET}$diff"
            ) // this reset is a hacky hack, but works!
      }
  }

  def equalToEq[T: Eq: Show](expected: T): Predicate[T] = Predicate {
    case actual if actual === expected => Assertion.successful
    case actual                        => Assertion.failed(show"$actual (actual) wasn't equal to $expected (expected).")
  }

  def select[A]: SelectPartiallyApplied[A] = new SelectPartiallyApplied[A](false)

  val successful: Predicate[Any] = PredicateT.const(Assertion.successful)
  def failed(message: String): Predicate[Any] = PredicateT.const(Assertion.failed(message))

  def isTrue(ifFalse: String): Predicate[Boolean] = Predicate {
    case true  => Assertion.successful
    case false => Assertion.failed(ifFalse)
  }

}

final class SelectPartiallyApplied[A] private[api] (private val dummy: Boolean) extends AnyVal {
  def apply[B, F[_]](f: A => B)(predicate: PredicateT[F, B]): PredicateT[F, A] = predicate.contramap(f)
}
