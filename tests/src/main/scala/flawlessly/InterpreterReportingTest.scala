package flawlessly

import flawless.data.Suite
import flawless.syntax._
import flawless._
import cats.implicits._
import flawless.eval.Reporter
import cats.effect.Resource
import cats.Monad

//Sync, because Bracket for WriterT isn't explicitly written
object InterpreterReportingTest {

  def apply[F[_]: Monad](implicit testReporter: TestReporter[F]): Suite[F] = {
    import testReporter._

    val simpleSuite = suite[WC]("suite 1") {
      tests(
        pureTest("test 1")(ensureEqual(1, 1)),
        pureTest("test 2")(ensureEqual(1, 1))
      )
    }

    import Reporter.Event._

    def simpleEvents(id: Int): List[LogEvent] =
      List[Reporter.Event[Int]](
        SuiteStarted("suite 1", id),
        TestStarted("test 1"),
        TestFinished("test 1"),
        TestStarted("test 2"),
        TestFinished("test 2"),
        SuiteFinished("suite 1", id, succeeded = true)
      ).map(LogEvent.Report(_))

    //todo: why are these instances not visible?
    def simpleResource(suite: Suite[WC]): Suite[WC] = Suite.resource(suite.pure[Resource[WC, *]])(wcBracket)

    //todo: these would be good property tests
    suite("InterpreterReportingTest") {
      tests(
        test("single suite") {
          ensure(simpleSuite, reported(simpleEvents(0)))
        },
        test("sequence of suites") {
          ensure(
            Suite.sequential(simpleSuite, simpleSuite),
            reported(
              List(LogEvent.ReplaceWith(0, 2)) ++
                simpleEvents(1) ++
                simpleEvents(2)
            )
          )
        },
        test("nested sequence of suites - left side") {
          ensure(
            Suite.sequential(Suite.sequential(simpleSuite), simpleSuite, simpleSuite),
            reported(
              List(LogEvent.ReplaceWith(0, 3)) ++
                simpleEvents(1) ++
                simpleEvents(2) ++
                simpleEvents(3)
            )
          )
        },
        test("nested sequence of suites - right side") {
          ensure(
            Suite.sequential(simpleSuite, simpleSuite, Suite.sequential(simpleSuite)),
            reported(
              List(LogEvent.ReplaceWith(0, 3)) ++
                simpleEvents(1) ++
                simpleEvents(2) ++
                simpleEvents(3)
            )
          )
        },
        test("nested sequence of suites - within parallel node") {
          val input = Suite.parallel(
            simpleSuite,
            simpleSuite,
            Suite.sequential(
              Suite.sequential(
                Suite.sequential(simpleSuite),
                simpleSuite
              ),
              Suite.sequential(simpleSuite, simpleSuite)
            )
          )(wcParallel)

          ensure(
            input,
            reported(
              List(LogEvent.ReplaceWith(0, 3)) ++
                simpleEvents(1) ++
                simpleEvents(2) ++
                List(LogEvent.ReplaceWith(3, 4)) ++
                simpleEvents(4) ++
                simpleEvents(5) ++
                simpleEvents(6) ++
                simpleEvents(7)
            )
          )
        },
        test("nested sequence of suites - both sides") {
          ensure(
            Suite.sequential(Suite.sequential(simpleSuite), Suite.sequential(simpleSuite)),
            reported(
              List(LogEvent.ReplaceWith(0, 2)) ++
                simpleEvents(1) ++
                simpleEvents(2)
            )
          )
        },
        test("resource doesn't allocate extra suites") {
          ensure(simpleResource(simpleSuite), reported(simpleEvents(0)))
        },
        test("sequence in resource") {
          val input = Suite.sequential(
            simpleSuite,
            simpleSuite,
            simpleResource {
              Suite.sequential(
                simpleSuite,
                simpleSuite
              )
            }
          )

          ensure(
            input,
            reported(
              List(
                LogEvent.ReplaceWith(0, 3).pure[List],
                simpleEvents(1),
                simpleEvents(2),
                LogEvent.ReplaceWith(3, 2).pure[List],
                simpleEvents(4),
                simpleEvents(5)
              ).flatten
            )
          )
        }
      )
    }
  }
}
