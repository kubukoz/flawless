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

  val dbTests: Suite[IO] = {
    val xa = for {
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
    } yield transactor

    Suite.resource[IO] {
      xa.map { transactor =>
        Suite.parSequence(NonEmptyList.of(new DoobieQueryTests(transactor).runSuite).combineN(10))
      }
    }
  }

  val runSequentials = Suite.sequential(
    Suite.parSequence(sequentialTests.map(_.runSuite)),
    dbTests
  )

  val runFlaky = FlakySuite.runSuite

  val runExpensives =
    Suite.parSequence[IO](NonEmptyList.fromListUnsafe(List.fill(10)(ExpensiveSuite)).map(_.runSuite))

  val runParallels = Suite.parSequence(parallelTests.map(_.runSuite))

  val testRange = Suite.sequential(runFlaky, runExpensives, runParallels, runSequentials)

  override def run(args: List[String]): IO[ExitCode] =
    runTests(args)(
      testRange
    )
}
