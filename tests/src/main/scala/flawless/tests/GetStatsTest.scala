package flawless.tests

import cats.data.NonEmptyList
import cats.effect.IO
import flawless._
import flawless.stats._
import cats.implicits._
import cats.Id

object GetStatsTest extends Suite {
  import RunStats.Stat
  import flawless.syntax._

  val runSuite: IO[SuiteResult] = {
    tests(
      pureTest("1 test: 1 succ / 0 fail") {

        val input = test[Id]("Example") {
          1 shouldBe 1
        }

        RunStats.fromSuites(NonEmptyList.one(input)) shouldBe RunStats(
          Stat(1, 1, 0),
          Stat(1, 1, 0),
          Stat(1, 1, 0)
        )
      },
      pureTest("1 test: 1 succ / 1 fail") {
        val input = test[Id]("Example") {
          (1 shouldBe 1) |+| (1 shouldBe 2)
        }

        RunStats.fromSuites(NonEmptyList.of(input)) shouldBe RunStats(
          Stat(1, 0, 1),
          Stat(1, 0, 1),
          Stat(2, 1, 1)
        )
      },
      pureTest("1 test: 1 succ / 2 fail") {
        val input = test[Id]("Example") {
          (1 shouldBe 1) |+| (1 shouldBe 2).combineN(2)
        }

        RunStats.fromSuites(NonEmptyList.of(input)) shouldBe RunStats(
          Stat(1, 0, 1),
          Stat(1, 0, 1),
          Stat(3, 1, 2)
        )
      }
    )
  }
  //todo property based tests - the total amount of each stat should be the sum of failed+succeeded
}
