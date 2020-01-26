package flawless.examples

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import flawless.dsl._
import flawless.SuiteClass
import flawless.data.Suite

object IOSuite extends SuiteClass[IO] {

  val service: MyService[IO] = MyService.instance

  override val runSuite: Suite[IO] = suite("IOSuite") {
    tests(
      test("job(1) and (2)")(
        service.job(1).map(ensureEqual(_, "I got 1 problems but a test ain't one")) |+|
          service.job(2).map(ensureEqual(_, "I got 2 problems but a test ain't one"))
      ),
      test("job(1-1000)")(
        NonEmptyList(1, (2 to 1000).toList).reduceMapM { n =>
          service.job(n).map { result =>
            ensureEqual(result, show"I got $n problems but a test ain't one") |+|
              ensureEqual(result.contains("500"), false)
          }
        }
      )
    )
  }
}
