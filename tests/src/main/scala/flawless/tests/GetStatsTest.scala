package flawless.tests

import cats.data.NonEmptyList
import flawless.stats._
import cats.implicits._
import flawless.data.neu.SuiteClass
import flawless.data.neu.Suite
import flawless.data.neu.dsl._

object GetStatsTest extends SuiteClass[Nothing] {
  import RunStats.Stat

  val runSuite: Suite[Nothing] = /* {
    tests(
      test("1 test: 1 succ / 0 fail") {
        pureTest("Example") {
          1 shouldBe 1
        }.interpret.map { input =>
          RunStats.fromSuites(NonEmptyList.one(input)) shouldBe RunStats(
            suite = Stat(1, 1, 0),
            test = Stat(1, 1, 0),
            assertion = Stat(1, 1, 0)
          )
        }
      },
      test("1 test: 1 succ / 1 fail") {
        pureTest("Example") {
          (1 shouldBe 1) |+| (1 shouldBe 2)
        }.interpret.map { input =>
          RunStats.fromSuites(NonEmptyList.of(input)) shouldBe RunStats(
            suite = Stat(1, 0, 1),
            test = Stat(1, 0, 1),
            assertion = Stat(2, 1, 1)
          )
        }
      },
      test("1 test: 1 succ / 2 fail") {
        pureTest("Example") {
          (1 shouldBe 1) |+| (1 shouldBe 2).combineN(2)
        }.interpret.map { input =>
          RunStats.fromSuites(NonEmptyList.of(input)) shouldBe RunStats(
            suite = Stat(1, 0, 1),
            test = Stat(1, 0, 1),
            assertion = Stat(3, 1, 2)
          )
        }
      }
    )
  } */
  ???
  //todo property based tests - the total amount of each stat should be the sum of failed+succeeded
}
