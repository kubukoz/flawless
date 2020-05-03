package flawless.eval

import cats.data.NonEmptyList
import cats.kernel.Eq
import cats.Show
import cats.implicits._
import flawless.eval.Output.Problem.Pending
import flawless.eval.Output.Problem.Failed

final case class Output(stats: RunStats, suites: NonEmptyList[Output.Suite])

object Output {
  implicit val show: Show[Output] = o => show"Output(stats = ${o.stats}, suite = ${o.suites})"
  implicit val eq: Eq[Output] = Eq.by(unapply(_).get)

  final case class Suite(name: String, tests: NonEmptyList[Test])

  object Suite {
    implicit val show: Show[Suite] = s => show"Suite(name = ${s.name}, tests = ${s.tests})"
    implicit val eq: Eq[Suite] = Eq.by(unapply(_).get)
  }

  //don't use EitherNel here - it kills magnolia
  final case class Test(name: String, problems: Either[NonEmptyList[Problem], Unit])

  object Test {
    implicit val show: Show[Test] = t => show"Test(name = ${t.name}, problems = ${t.problems})"
    implicit val eq: Eq[Test] = Eq.by(unapply(_).get)
  }

  sealed trait Problem extends Product with Serializable {
    def toEither: Either[Unit, String] = fold(().asLeft, _.asRight)

    def fold[A](pending: A, failed: String => A): A = this match {
      case Pending        => pending
      case Failed(reason) => failed(reason)
    }
  }

  object Problem {
    case object Pending extends Problem
    final case class Failed(reason: String) extends Problem

    implicit val show: Show[Problem] = {
      case Pending        => "Pending"
      case Failed(reason) => show"Failed($reason)"
    }

    implicit val eq: Eq[Problem] = Eq.by {
      case Pending     => true.asRight
      case Failed(msg) => msg.asLeft
    }
  }
}
