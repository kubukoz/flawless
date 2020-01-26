package flawless.api

import flawless.data._
import cats.implicits._
import com.softwaremill.diffx.Diff
import com.softwaremill.diffx.DiffResult
import cats.Show
import cats.Order
import flawless.Predicate

trait AllPredicates {

  def greaterThan[A: Order: Show](another: A): Predicate[A] =
    a => if (a > another) Assertion.successful else Assertion.failed(show"$a was not greater than $another")

  def equalTo[T: Diff: Show](another: T): Predicate[T] = {
    implicit val showDiff: Show[DiffResult] = _.show

    a =>
      Diff[T].apply(a, another) match {
        case diff if diff.isIdentical => Assertion.successful
        case diff                     => Assertion.failed(show"$a (actual) was not equal to $another (expected). Diff: $diff")
      }
  }

  val successful: Predicate[Any] = _ => Assertion.successful
  def failed(message: String): Predicate[Any] = _ => Assertion.failed(message)

  def isTrue(ifFalse: String): Predicate[Boolean] = {
    case true  => Assertion.successful
    case false => Assertion.failed(ifFalse)
  }
}
