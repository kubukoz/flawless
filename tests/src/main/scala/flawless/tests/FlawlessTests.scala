package flawless.tests

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.data.NonEmptyList
import cats.implicits._
import flawless._

//test runner for the whole module
object FlawlessTests extends IOApp {

  def run(args: List[String]): IO[ExitCode] = runTests(args) {
    val parallelTests = NonEmptyList.of(
      GetStatsTest,
      StructureTests,
      SimplePureTest
    )

    parallelTests.parTraverse(_.runSuite)
  }
}
