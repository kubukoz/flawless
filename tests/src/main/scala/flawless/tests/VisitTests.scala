package flawless.tests

object VisitTests
/*
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO
import flawless.Assertion.Successful
import flawless.Assertions
import flawless.Suite
import flawless.SuiteResult
import flawless.Tests
import cats.effect.concurrent.Ref

object VisitTests extends Suite {
  import flawless.syntax._

  override val runSuite: Tests[SuiteResult] = tests(
    test("doesn't visit pure tests") {
      val example = pureTest("hello")(Assertions(Successful))

      example.via(_ => IO.raiseError(new Throwable("failed"))).interpret.map {
        _.isSuccessful shouldBe true
      }
    },
    test("visits IO tests") {
      val example = test("hello")(Assertions(Successful).pure[IO])

      example.via(_ => IO.raiseError(new Throwable("failed"))).interpret.attempt.map {
        _.isLeft shouldBe true
      }
    },
    test("visits nested IOs") {
      Ref[IO].of(List.empty[String]).flatMap { ref =>
        val baseIO: IO[Unit] = ref.update("foo" :: _)
        val secondIO: IO[Unit] = ref.update("boo" :: _)

        val example =
          Tests.sequence(NonEmptyList.of(test("hello")(baseIO.map(_ => 1 shouldBe 1)), test("hello2")(baseIO.map(_ => 1 shouldBe 1))))

        example.via(b => secondIO *> b).interpret *> ref.get.map { log =>
          (log.size shouldBe 4) |+| (log shouldBe List("foo", "boo", "foo", "boo"))
        }
      }
    }
  )
}
 */
