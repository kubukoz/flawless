package foo

//here goes the demo!

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import flawless._

object ExampleTest extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val parallelTests = NonEmptyList.of(
      FirstSuite,
      FirstSuite
    )

    val sequentialTests = NonEmptyList.of(
      FirstSuite
    )

    runTests(args)(
      parallelTests.parTraverse(_.runSuite) |+| sequentialTests.traverse(_.runSuite)
    )
  }
}

import flawless.syntax.pure._

object FirstSuite extends PureSuite {
  val service: MyService = LiveMyService

  override val runSuitePure: PureTest[SuiteResult] = {
    test("job(1) and (2)")(
      service.job(1).shouldBe("I got 1 problems but a test ain't one") |+|
        service.job(2).shouldBe("I got 2 problems but a test ain't one")
    ) |+|
      test("job(1-1000)")(
        //this test is pretty useless tbh, don't write tests like that
        NonEmptyList(1, (2 to 1000).toList).reduceMap { n =>
          val result = service.job(n)
          result.shouldBe(show"I got $n problems but a test ain't one") |+|
            result.contains("500").shouldBe(false)
        }
      )
  }
}

trait MyService {
  def job(i: Int): String
}

object LiveMyService extends MyService {
  override def job(i: Int): String = "I got " + i + " problems but a test ain't one"
}
