package flawless.tests

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import flawless._
import flawless.data.neu.Suites
import flawless.data.neu.predicates.all._
import cats.implicits._
import flawless.data.neu.TestApp

//test runner for the whole module
object FlawlessTests extends IOApp with TestApp {

  def run(args: List[String]): IO[ExitCode] = runTests(args) {
    // Suites.parallel(
    // GetStatsTest,
    // VisitTests,
    // new ResourceTests
    // )

    import flawless.data.neu.dsl._

    import scala.concurrent.duration._
    val helloSuite = suite("Hello world") {
      tests(
        test("world") {
          IO.sleep(10.millis).map { _ =>
            ensure(1, equalTo(3)) <+> ensure(2, equalTo(2))
          }
        },
        testMonadic[IO]("monadic") { assertions =>
          for {
            _ <- assertions.addAll(ensure(1, equalTo(5)))
            _ <- IO.sleep(200.millis)
            _ <- assertions.addAll(ensure(2, equalTo(4)))
          } yield ()
        }
      )
    }

    Suites.parallel(GetStatsTest.runSuite.toSuites, helloSuite.toSuites)
  }
}
