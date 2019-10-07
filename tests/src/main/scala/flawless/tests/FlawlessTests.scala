package flawless.tests

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless._
import flawless.data.neu.Suites
import flawless.data.neu.predicates.all._
import cats.implicits._
import flawless.data.neu.TestApp

//test runner for the whole module
object FlawlessTests extends IOApp with TestApp {

  def run(args: List[String]): IO[ExitCode] = runTests[IO](args) {
    // Suites.parallel(
    // GetStatsTest,
    // VisitTests,
    // new ResourceTests
    // )

    GetStatsTest.runSuite.toSuites
  }
}
