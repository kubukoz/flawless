package flawless.tests

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO
import flawless.Assertion.Successful
import flawless.Assertions
import flawless.Suite
import flawless.SuiteResult
import flawless.Tests

object VisitTests extends Suite {
  import flawless.syntax._

  override val runSuite: Tests[SuiteResult] = tests(
    test("doesn't visit pure tests") {
      val example = pureTest("hello")(Assertions(Successful))

      example.visit(_ => IO.raiseError(new Throwable("failed"))).interpret.map {
        _.isSuccessful shouldBe true
      }
    },
    test("visits IO tests") {
      val example = test("hello")(Assertions(Successful).pure[IO])

      example.visit(_ => IO.raiseError(new Throwable("failed"))).interpret.attempt.map {
        _.isLeft shouldBe true
      }
    }
  )
}
