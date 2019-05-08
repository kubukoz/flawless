package flawless.examples

//here goes the demo!

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import flawless._
import cats.effect.Console.io._

import flawless.examples.doobie.DoobieQueryTests
import _root_.doobie.Transactor

object ExampleTests extends IOApp {

  //flaky test detector
  def runUntilFailed(test: IOTest[SuiteResult]): IOTest[SuiteResult] = {
    fs2.Stream
      .repeatEval(test)
      .zipWithIndex
      .dropWhile {
        case (suite, _) => suite.results.forall(_.assertions.value.forall(_.isSuccessful))
      }
      .head
      .evalMap {
        case (failure, successes) =>
          putStrLn(show"Suite failed after ${successes + 1} successes").as(failure)
      }
      .compile
      .lastOrError
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val parallelTests = NonEmptyList.of(
      FirstSuite,
      FirstSuite,
      IOSuite
    )

    val sequentialTests = NonEmptyList.of(
      FirstSuite,
      IOSuite
    )

    val dbTests = {
      val xa = Transactor.fromDriverManager[IO](
        "org.postgresql.Driver",
        "jdbc:postgresql://localhost:5432/postgres",
        "postgres",
        "postgres"
      )
      NonEmptyList.fromListUnsafe(List.fill(10)(new DoobieQueryTests(xa)))
    }

    runTests(args)(
      runUntilFailed(FlakySuite.runSuite).map(NonEmptyList.one) |+|
        NonEmptyList.fromListUnsafe(List.fill(10)(ExpensiveSuite)).parTraverse(_.runSuite) |+| parallelTests
        .parTraverse(
          _.runSuite
        ) |+| sequentialTests.traverse(_.runSuite) |+| dbTests.parTraverse(_.runSuite)
    )
  }
}
