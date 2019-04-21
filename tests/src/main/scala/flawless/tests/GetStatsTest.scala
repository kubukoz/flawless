package flawless.tests

import cats.data.NonEmptyList
import flawless._
import cats.implicits._

object GetStatsTest extends PureSuite {
  import flawless.syntax.pure._
  import RunStats.Stat

  val runSuitePure: PureTest[SuiteResult] = tests(
    test("1 test: 1 succ / 0 fail") {
      val input = test("Example") {
        1 shouldBe 1
      }

      getStats(NonEmptyList.of(input)) shouldBe RunStats(
        Stat(1, 1, 0),
        Stat(1, 1, 0),
        Stat(1, 1, 0)
      )
    },
    test("1 test: 1 succ / 1 fail") {
      val input = test("Example") {
        (1 shouldBe 1) |+| (1 shouldBe 2)
      }

      getStats(NonEmptyList.of(input)) shouldBe RunStats(
        Stat(1, 0, 1),
        Stat(1, 0, 1),
        Stat(2, 1, 1)
      )
    },
    test("1 test: 1 succ / 2 fail") {
      val input = test("Example") {
        (1 shouldBe 1) |+| (1 shouldBe 2).combineN(2)
      }

      getStats(NonEmptyList.of(input)) shouldBe RunStats(
        Stat(1, 0, 1),
        Stat(1, 0, 1),
        Stat(3, 1, 2)
      )
    }
  )
  //todo property based tests - the total amount of each stat should be the sum of failed+succeeded
}
