package flawless.tests

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless._
import flawless.TestApp

//test runner for the whole module
object FlawlessTests extends IOApp with TestApp {

  def run(args: List[String]): IO[ExitCode] = runTests[IO](args) {
    GetStatsTest.runSuite.toSuites
  }
}
