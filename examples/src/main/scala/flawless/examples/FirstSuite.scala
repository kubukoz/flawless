package flawless.examples

import cats.Id
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO
import flawless.{IOTest, Suite, SuiteResult}

object FirstSuite extends Suite {
  val service: MyService[Id] = MyService.instance

  import flawless.syntax._

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
