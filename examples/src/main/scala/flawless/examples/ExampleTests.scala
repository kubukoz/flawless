package flawless.examples

//here goes the demo!

import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless.data._
import flawless._
import flawless.examples.doobie.DoobieQueryTests
import _root_.doobie.util.ExecutionContexts
import _root_.doobie.hikari.HikariTransactor
import cats.effect.Blocker
import flawless.TestApp
import cats.effect.Resource
import cats.implicits._

object ExampleTests extends IOApp with TestApp {

  val sequentialTests = NonEmptyList.of(
    FirstSuite,
    IOSuite,
    SimplePureTest
  )

  val parallelTests = NonEmptyList.of(
    FirstSuite,
    IOSuite
  )

  val dbTests: Resource[IO, NonEmptyList[Suites[IO]]] = {
    for {
      connectEc <- ExecutionContexts.fixedThreadPool[IO](10)
      blocker   <- Blocker[IO]
      transactor <- HikariTransactor.newHikariTransactor[IO](
                     "org.postgresql.Driver",
                     "jdbc:postgresql://localhost:5432/postgres",
                     "postgres",
                     "postgres",
                     connectEc,
                     blocker
                   )
    } yield NonEmptyList.of(new DoobieQueryTests(transactor).runSuite.toSuites).combineN(10)
  }

  val runSequentials = Suites.sequential(
    Suites.parSequence(sequentialTests.map(_.runSuite.toSuites)),
    Suites.resource(dbTests.map(Suites.parSequence(_)))
  )

  val runFlaky = FlakySuite.runSuite.toSuites

  val runExpensives =
    Suites.parSequence(NonEmptyList.fromListUnsafe(List.fill(10)(ExpensiveSuite)).map(_.runSuite.toSuites: Suites[IO]))

  val runParallels = Suites.parSequence(parallelTests.map(_.runSuite.toSuites))

  val testRange = Suites.sequential(runFlaky, runExpensives, runParallels, runSequentials)

  override def run(args: List[String]): IO[ExitCode] =
    runTests(args)(
      testRange
    )
}
