package flawless.tests

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless._
import flawless.TestApp
import flawless.data.Suites

//test runner for the whole module
object FlawlessTests extends IOApp with TestApp {

  def run(args: List[String]): IO[ExitCode] =
    MyAlg.syncInstance[IO].flatMap { implicit alg =>
      runTests[IO](args) {
        Suites.sequential(
          GetStatsTest.runSuite.toSuites,
          ReporterTest.runSuite.toSuites,
          new TaglessTest[IO].runSuite.toSuites
        )
      }
    }
}
