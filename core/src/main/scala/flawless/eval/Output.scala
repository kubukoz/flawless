package flawless.eval

import cats.data.NonEmptyList
import cats.data.EitherNel
import cats.kernel.Eq
import cats.implicits._
import cats.Show

final case class Output(stats: RunStats, suites: NonEmptyList[Output.Suite])

object Output {
  final case class Suite(name: String, tests: NonEmptyList[Test])

  object Suite {
    implicit val show: Show[Suite] = s => show"Suite(name = ${s.name}, tests = ${s.tests})"
    implicit val eq: Eq[Suite] = Eq.by(unapply(_).get)
  }

  final case class Test(name: String, problems: EitherNel[String, Unit])

  object Test {
    implicit val show: Show[Test] = t => show"Test(name = ${t.name}, problems = ${t.problems})"
    implicit val eq: Eq[Test] = Eq.by(unapply(_).get)
  }

  implicit val show: Show[Output] = o => show"Output(stats = ${o.stats}, suite = ${o.suites})"
  implicit val eq: Eq[Output] = Eq.by(unapply(_).get)
}
