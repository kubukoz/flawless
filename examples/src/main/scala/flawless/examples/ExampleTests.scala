package flawless.examples

//here goes the demo!

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import flawless._
import cats.effect.Console.io._

import flawless.examples.doobie.DoobieQueryTests
import _root_.doobie.util.ExecutionContexts
import _root_.doobie.hikari.HikariTransactor

object ExampleTests extends IOApp {

  //flaky test detector
  def runUntilFailed(test: IOTest[SuiteResult]): IOTest[SuiteResult] = {
    fs2.Stream
      .repeatEval(test)
      .zipWithIndex
      .find {
        case (suite, _) => suite.results.exists(_.assertions.value.exists(_.isFailed))
      }
      .evalMap {
        case (failure, failureIndex) =>
          putStrLn(show"Suite failed after ${failureIndex + 1} successes").as(failure)
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
      for {
        connectEc  <- ExecutionContexts.fixedThreadPool[IO](10)
        transactEc <- ExecutionContexts.cachedThreadPool[IO]
        transactor <- HikariTransactor.newHikariTransactor[IO](
          "org.postgresql.Driver",
          "jdbc:postgresql://localhost:5432/postgres",
          "postgres",
          "postgres",
          connectEc,
          transactEc
        )
      } yield NonEmptyList.fromListUnsafe(List.fill(10)(new DoobieQueryTests(transactor)))
    }

    runTests(args)(
      runUntilFailed(FlakySuite.runSuite).map(NonEmptyList.one) |+|
        NonEmptyList
          .fromListUnsafe(List.fill(10)(ExpensiveSuite))
          .parTraverse(_.runSuite) |+| parallelTests.parTraverse(
        _.runSuite
      ) |+| sequentialTests.traverse(_.runSuite) |+| dbTests.use(
        _.parTraverse(_.runSuite)
      )
    )
  }
}