package flawless.eval

import cats.data.NonEmptyList
import cats.data.EitherNel

final case class Output(stats: RunStats, suites: NonEmptyList[Output.Suite])

object Output {
  final case class Suite(name: String, tests: NonEmptyList[Test])
  final case class Test(name: String, problems: EitherNel[String, Unit])
}
