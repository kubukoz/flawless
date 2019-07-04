package flawless.tests

import flawless.Suite
import flawless.SuiteResult
import flawless.Tests
import cats.effect.IO
import cats.effect.concurrent.Deferred
import cats.effect.ExitCase
import cats.effect.Resource
import cats.implicits._
import cats.effect.ContextShift

class ResourceTests(implicit cs: ContextShift[IO]) extends Suite {
  import flawless.syntax._
  import flawless.syntax.idgaf._

  def mkTest(use: IO[SuiteResult]) =
    for {
      resourceFinalized <- Deferred[IO, ExitCase[Throwable]]
    } yield {
      val res = Resource.makeCase(IO.unit)((_, ec) => resourceFinalized.complete(ec))
      val test = Tests.resource(res).use(_ => Tests.liftIO(use)).interpret
      (test, resourceFinalized.get)
    }

  val runSuite: Tests[SuiteResult] = tests(
    test("Tests.resource cleans up and forwards error on errors in use") {
      val e = new Throwable("oops")

      mkTest(IO.raiseError(e)).flatMap {
        case (test, resourceFinalized) =>
          test.attempt *> resourceFinalized
      }.map(_.shouldBe(ExitCase.error(e)))
    },
    test("Tests.resource cleans up and forwards cancelation on cancelation in use") {
      Deferred[IO, Unit].flatMap {
        testStarted =>
          Deferred[IO, ExitCase[Throwable]].flatMap {
            testFinalized =>
              val nonTerminatingTest = mkTest {
                testStarted.complete(()).bracketCase(_ => IO.never)((_, ec) => testFinalized.complete(ec))
              }

              val runTest = nonTerminatingTest.flatMap {
                case (test, resourceFinalized) =>
                  //run test, wait until it starts, then cancel and wait for finalization
                  test.start.flatMap { testStarted.get *> _.cancel } *> resourceFinalized
              }

              (runTest, testFinalized.get).mapN {
                case (resourceEC, testEC) =>
                  resourceEC.shouldBe(ExitCase.canceled) |+| testEC.shouldBe(ExitCase.canceled)
              }
          }
      }
    }
  )
}
