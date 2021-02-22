package flawless.examples

//here goes the demo!

import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless._
// import flawless.examples.doobie.DoobieQueryTests

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

  // TODO: Enable again when doobie publishes for CE3
  // val dbTests: Suite[IO] = {
  //   val xa = for {
  //     connectEc  <- ExecutionContexts.fixedThreadPool[IO](10)
  //     blocker    <- Blocker[IO]
  //     transactor <- HikariTransactor.newHikariTransactor[IO](
  //                     "org.postgresql.Driver",
  //                     "jdbc:postgresql://localhost:5432/postgres",
  //                     "postgres",
  //                     "postgres",
  //                     connectEc,
  //                     blocker
  //                   )
  //   } yield transactor

  //   Suite
  //     .resource[IO] {
  //       xa.map { transactor =>
  //         new DoobieQueryTests(transactor).runSuite.parCombineN(5) //5 suites per allocation
  //       }
  //     }
  //     .parCombineN(2) //2 allocations
  // }

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
