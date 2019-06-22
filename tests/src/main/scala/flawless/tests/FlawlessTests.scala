package flawless.tests

import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless._

//test runner for the whole module
object FlawlessTests extends IOApp {

  def run(args: List[String]): IO[ExitCode] = runTests(args) {
    val parallelTests = NonEmptyList.of(
      new GetStatsTest
    )

    Tests.parallel(parallelTests.map(_.runSuite))
  }
}
