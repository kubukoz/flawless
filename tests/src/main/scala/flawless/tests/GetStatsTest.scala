package flawless.tests

import cats.data.NonEmptyList
import cats.effect.IO
import flawless._
import flawless.stats._
import cats.implicits._
import cats.Id
import cats.effect.ContextShift

class GetStatsTest(implicit cs: ContextShift[IO]) extends Suite {
  import RunStats.Stat
  import flawless.syntax._

  val runSuite: Tests[SuiteResult] = {
    tests(
      test("1 test: 1 succ / 0 fail") {

        pureTest("Example") {
          1 shouldBe 1
        }.interpret0.map { input =>
          RunStats.fromSuites(NonEmptyList.one(input)) shouldBe RunStats(
            Stat(1, 1, 0),
            Stat(1, 1, 0),
            Stat(1, 1, 0)
          )
        }
      },
      test("1 test: 1 succ / 1 fail") {
        pureTest("Example") {
          (1 shouldBe 1) |+| (1 shouldBe 2)
        }.interpret0.map { input =>
          RunStats.fromSuites(NonEmptyList.of(input)) shouldBe RunStats(
            Stat(1, 0, 1),
            Stat(1, 0, 1),
            Stat(2, 1, 1)
          )
        }
      },
      test("1 test: 1 succ / 2 fail") {
        pureTest("Example") {
          (1 shouldBe 1) |+| (1 shouldBe 2).combineN(2)
        }.interpret0.map { input =>
          RunStats.fromSuites(NonEmptyList.of(input)) shouldBe RunStats(
            Stat(1, 0, 1),
            Stat(1, 0, 1),
            Stat(3, 1, 2)
          )
        }
      }
    )
  }
  //todo property based tests - the total amount of each stat should be the sum of failed+succeeded
}
