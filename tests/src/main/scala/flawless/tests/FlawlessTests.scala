package flawless.tests

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import flawless._

//test runner for the whole module
object FlawlessTests extends IOApp {

  def run(args: List[String]): IO[ExitCode] = runTests(args) {
    val parallelTests = NonEmptyList.of(
      GetStatsTest
    )

    parallelTests.parTraverse(_.runSuite)
  }
}
