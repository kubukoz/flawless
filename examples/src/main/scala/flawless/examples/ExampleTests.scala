package flawless.examples

//here goes the demo!

import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import flawless._

import flawless.examples.doobie.DoobieQueryTests
import _root_.doobie.util.ExecutionContexts
import _root_.doobie.hikari.HikariTransactor
import cats.effect.Console.io._

object ExampleTests extends IOApp {

  //flaky test detector
  def deflake(test: Tests[SuiteResult]): Tests[SuiteResult] =
    test.visit { action =>
      fs2.Stream
        .repeatEval(action)
        .zipWithIndex
        .find {
          case (suite, _) => suite.isFailed
        }
        .evalMap {
          case (failure, failureIndex) =>
            putStrLn(show"Suite failed after ${failureIndex + 1} successes").as(failure)
        }
        .compile
        .lastOrError
    }

  override def run(args: List[String]): IO[ExitCode] = {
    val sequentialTests = NonEmptyList.of(
      FirstSuite,
      IOSuite,
      SimplePureTest
    )

    val parallelTests = NonEmptyList.of(
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
      } yield NonEmptyList.fromListUnsafe(List.fill(10)(new DoobieQueryTests(transactor).runSuite))
    }

    val runSequentials = (
      Tests.parSequence(sequentialTests.map(_.runSuite))
        |+| Tests.liftResource(dbTests)(Tests.parSequence(_))
    )

    val runFlaky = deflake(FlakySuite.runSuite).liftA[NonEmptyList]

    val runExpensives =
      Tests.parSequence(NonEmptyList.fromListUnsafe(List.fill(10)(ExpensiveSuite)).map(_.runSuite))

    val runParallels = Tests.parSequence(parallelTests.map(_.runSuite))

    runTests(args)(
      runFlaky |+| runExpensives |+| runParallels |+| runSequentials
    )
  }
}
