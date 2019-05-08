package flawless.examples

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._

import flawless.{IOTest, Suite, SuiteResult}

object IOSuite extends Suite {

  val service: MyService[IO] = MyService.instance

  import flawless.syntax.io._

  override val runSuite: IOTest[SuiteResult] = {
    test("job(1) and (2)")(
      service.job(1).map(_.shouldBe("I got 1 problems but a test ain't one")) |+|
        service.job(2).map(_.shouldBe("I got 2 problems but a test ain't one"))
    ) |+|
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
