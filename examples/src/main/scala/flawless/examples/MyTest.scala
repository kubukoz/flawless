package foo

//here goes the demo!

import cats.Applicative
import cats.Id
import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import flawless._
import cats.NonEmptyParallel
import flawless.syntax.TestCompiler

object ExampleTest extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val parallelTests = NonEmptyList.of(
      FirstSuite,
      FirstSuite,
      new IOSuite
    )

    val sequentialTests = NonEmptyList.of(
      FirstSuite,
      new IOSuite
    )

    runTests(args)(
      parallelTests.parTraverse(_.runSuite) |+| sequentialTests.traverse(_.runSuite)
    )
  }
}

object FirstSuite extends Suite {
  val service: MyService[Id] = new MyServiceImpl

  import flawless.syntax.pure._

  override val runSuite: IOTest[SuiteResult] = IO {
    test("job(1) and (2)")(
      service.job(1).shouldBe("I got 1 problems but a test ain't one") |+|
        service.job(2).shouldBe("I got 2 problems but a test ain't one")
    ) |+|
      test("job(1-1000)")(
        NonEmptyList(1, (2 to 1000).toList).reduceMap { n =>
          val result = service.job(n)
          result.shouldBe(show"I got $n problems but a test ain't one") |+|
            result.contains("500").shouldBe(false)
        }
      )
  }
}

class IOSuite[G[_]](implicit nep: NonEmptyParallel[IO, G], compiler: TestCompiler[IO]) extends Suite {

  val service: MyService[IO] = new MyServiceImpl[IO]

  import flawless.syntax.io._

  override val runSuite: IOTest[SuiteResult] = {
    test("job(1) and (2)")(
      service.job(1).map(_.shouldBe("I got 1 problems but a test ain't one")) |+|
        service.job(2).map(_.shouldBe("I got 2 problems but a test ain't one"))
    ) |&|
      test("job(1-1000)")(
        NonEmptyList(1, (2 to 1000).toList).reduceMapM { n =>
          service.job(n).map { result =>
            result.shouldBe(show"I got $n problems but a test ain't one") |+|
              result.contains("500").shouldBe(false)
          }
        }
      )
  }
}

trait MyService[F[_]] {
  def job(i: Int): F[String]
}

class MyServiceImpl[F[_]: Applicative] extends MyService[F] {

  override def job(i: Int): F[String] =
    ("I got " + i + " problems but a test ain't one").pure[F]
}
