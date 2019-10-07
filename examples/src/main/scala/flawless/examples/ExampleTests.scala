package flawless.examples

//here goes the demo!

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless.data._
import flawless._
import flawless.examples.doobie.DoobieQueryTests
import _root_.doobie.util.ExecutionContexts
import _root_.doobie.hikari.HikariTransactor
import cats.Foldable
import cats.effect.Blocker
import flawless.TestApp
import cats.effect.Resource
import fs2.Pipe
import cats.implicits._

object ExampleTests extends IOApp with TestApp {

  //flaky test detector
  def deflake(maxRetries: Option[Long] = Some(10000L)): Test[IO] => Test[IO] =
    _.via {
      def counted[A]: Pipe[IO, A, A] = maxRetries match {
        case Some(max) => _.take(max)
        case None      => identity
      }
      def successful[F[_]: Foldable](result: F[Assertion]): Boolean = result.forall(_.isSuccessful)

      _.via { io =>
        val findFirstFailure = fs2.Stream
          .repeatEval(io)
          .through(counted)
          .zipWithIndex
          .find {
            case (result, _) => !successful(result)
          }
          .compile
          .last
          .flatMap {
            case Some((failure, failureIndex)) => console.putStrLn(show"Suite failed after ${failureIndex + 1} successes").as(failure.some)
            case None                          => console.putStrLn(show"Didn't find flaky suite after $maxRetries successes").as(none)
          }

        io.flatMap { firstResult =>
          if (!successful(firstResult)) firstResult.pure[IO]
          else OptionT(findFirstFailure).getOrElse(firstResult)
        }
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
      testRange //.viaTest(deflake()) //.via(modifiers.catching)
    )
}
