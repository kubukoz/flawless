package flawlessly

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless._
import flawless.TestApp
import flawless.data.Suite

//test runner for the whole module
object FlawlessTests extends IOApp with TestApp {

  def run(args: List[String]): IO[ExitCode] =
    runTests[IO](args) {
      val tagless = Suite.suspend {
        MyAlg.syncInstance[IO].map { implicit alg =>
          new TaglessTest[IO].runSuite
        }
      }

      Suite.sequential(
        GetStatsTest.runSuite,
        ReporterTest.runSuite,
        tagless,
        new TaglessTestLocalResource[IO].runSuite
      )
    }
}