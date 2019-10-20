package flawless.examples

import cats.Id
import cats.implicits._
import cats.data.NonEmptyList
import flawless.dsl._
import flawless.SuiteClass
import flawless.data.Suite

object FirstSuite extends SuiteClass[Nothing] {
  val service: MyService[Id] = MyService.instance

  override val runSuite: Suite[Nothing] = suite("FirstSuite") {
    pureTest("job(1) and (2)")(
      ensureEqual[String](service.job(1), "I got 1 problems but a test ain't one") |+|
        ensureEqual[String](service.job(2), "I got 2 problems but a test ain't one")
    ) |+|
      pureTest("job(1-1000)")(
        NonEmptyList(1, (2 to 1000).toList).reduceMap { n =>
          val result = service.job(n)
          ensureEqual(result, show"I got $n problems but a test ain't one") |+|
            ensureEqual(result.contains("500"), false)
        }
      )
  }
}
