package flawless.examples

//here goes the demo!

import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless._
import _root_.doobie.util.ExecutionContexts
import flawless.examples.doobie.DoobieQueryTests
import _root_.doobie.hikari.HikariTransactor

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
      connectEc  <- ExecutionContexts.fixedThreadPool[IO](10)
      transactor <- HikariTransactor.newHikariTransactor[IO](
                      "org.postgresql.Driver",
                      "jdbc:postgresql://localhost:5432/postgres",
                      "postgres",
                      "postgres",
                      connectEc
                    )
    } yield transactor

    Suite
      .resource[IO] {
        xa.map { transactor =>
          new DoobieQueryTests(transactor).runSuite.parCombineN(5) // 5 suites per allocation
        }
      }
      .parCombineN(2) // 2 allocations
  }

  override def run(args: List[String]): IO[ExitCode] =
    runTests(args)(
      Suite.sequential(
        FlakySuite.runSuite,
        ExpensiveSuite.runSuite.parCombineN[IO](10),
        Suite.parSequence(parallelTests.map(_.runSuite)).parCombineN(10),
        Suite.sequence(sequentialTests.map(_.runSuite))
        // dbTests
      )
    )

}
