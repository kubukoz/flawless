package foo

//here goes the demo!

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import flawless._

object ExampleTest extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val parallelTests = NonEmptyList.of(
      FirstSpec,
      FirstSpec
    )

    val sequentialTests = NonEmptyList.of(
      FirstSpec,
      FirstSpec
    )

    runTests(args)(
      (parallelTests.parTraverse(_.runSpec) |+| sequentialTests.traverse(_.runSpec))
    )
  }
}

import flawless.syntax.pure._

object FirstSpec extends PureSpec {
  val service: MyService = LiveMyService

  override val runPure: PureTest[SpecResult] = {
    test("job(1) and (2)")(
      service.job(1).shouldBe("I got 1 problems but a test ain't one") |+|
        service.job(2).shouldBe("I got 2 problems but a test ain't one")
    ) |+|
      test("job(3) and (4)")(
        service.job(3).shouldBe("I got 3 problems but a test ain't one") |+|
          service.job(4).shouldBe("I got 4 problems but a test ain't one")
      )
  }
}

trait MyService {
  def job(i: Int): String
}

object LiveMyService extends MyService {
  override def job(i: Int): String = "I got " + i + " problems but a test ain't one"
}
