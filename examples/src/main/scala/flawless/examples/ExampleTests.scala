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
import fs2.Pipe
import cats.data.OptionT

object ExampleTests extends IOApp {
  import flawless.syntax._

  //flaky test detector
  def deflake(maxRetries: Option[Long] = Some(10000L)): IO[SuiteResult] => IO[SuiteResult] =
    io => {
      def counted[A]: Pipe[IO, A, A] = maxRetries match {
        case Some(max) => _.take(max)
        case None      => identity
      }

      val findFirstFailure = fs2.Stream
        .repeatEval(io)
        .through(counted)
        .zipWithIndex
        .find {
          case (suite, _) => suite.isFailed
        }
        .compile
        .last
        .flatMap {
          case Some((failure, failureIndex)) =>
            putStrLn(show"Suite failed after ${failureIndex + 1} successes").as(failure.some)
          case None =>
            putStrLn(show"Didn't find flaky suite after $maxRetries successes").as(none)
        }

      io.flatMap { firstResult =>
        if (!firstResult.isSuccessful) firstResult.pure[IO]
        else OptionT(findFirstFailure).getOrElse(firstResult)
      }
    }

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
      |+| Tests.resource(dbTests).use(Tests.parSequence(_)).combineN(2)
  )

  val runFlaky = FlakySuite.runSuite.map(NonEmptyList.one)

  val runExpensives =
    Tests.parSequence(NonEmptyList.fromListUnsafe(List.fill(10)(ExpensiveSuite)).map(_.runSuite))

  val runParallels = Tests.parSequence(parallelTests.map(_.runSuite))

  val testRange = runFlaky |+| runExpensives |+| runParallels |+| runSequentials

  override def run(args: List[String]): IO[ExitCode] =
    runTests(args)(
      testRange.via(deflake()).via(modifiers.catching)
    )
}
