package flawlessly

import cats.implicits._
import cats.Id
import flawless._
import flawless.syntax._
import flawless.eval.RunStats
import cats.data.NonEmptyList
import flawless.data.Assertion

object RunStatsTest extends SuiteClass[NoEffect] {
  import RunStats.Stat

  def runStats(predicate: Predicate[RunStats]): Predicate[Suite[Id]] =
    select[Suite[Id]](RunStats.fromSuites(_))(predicate)

  object examples {
    val successful = ensure(1, equalTo(1))
    val failed = ensure(1, equalTo(2))
    val pending = Assertion.pending
  }

  import examples._

  private val singleSuiteTests = tests(
    pureTest("1 test: 1 succ / 0 fail") {
      val input = suite[NoEffect]("foo") {
        pureTest("Example")(successful)
      }

      ensure(
        input,
        runStats(
          equalTo(
            RunStats(
              suite = Stat(1, 0, 0),
              test = Stat(1, 0, 0),
              assertion = Stat(1, 0, 0)
            )
          )
        )
      )
    },
    pureTest("1 test: 1 succ / 1 fail") {
      val input = suite[NoEffect]("foo") {
        pureTest("Example") {
          successful |+| failed
        }
      }

      ensure(
        input,
        runStats(
          equalTo(
            RunStats(
              suite = Stat(0, 0, 1),
              test = Stat(0, 0, 1),
              assertion = Stat(1, 0, 1)
            )
          )
        )
      )
    },
    pureTest("1 test: 1 succ / 2 fail") {
      val input = suite[NoEffect]("foo") {
        pureTest("Example") {
          successful |+| failed.combineN(2)
        }
      }

      ensure(
        input,
        runStats(
          equalTo(
            RunStats(
              suite = Stat(0, 0, 1),
              test = Stat(0, 0, 1),
              assertion = Stat(1, 0, 2)
            )
          )
        )
      )
    }
  )

  private val multiSuiteTests = {
    val input = suite[NoEffect]("foo") {
      tests(
        pureTest("Example")(successful |+| successful),
        pureTest("Example 2")(failed |+| failed),
        pureTest("Example 3")(failed |+| successful)
      )
    }

    val expected = RunStats(
      suite = Stat(0, 0, 1),
      test = Stat(1, 0, 2),
      assertion = Stat(3, 0, 3)
    )

    def testSuites(suiteCount: Int) =
      pureTest(show"$suiteCount suite(s) with 1 succ test, 1 mixed (failed) test, 1 failed test each") {
        ensure(
          input.widenF[Id].combineN(suiteCount),
          runStats(
            equalTo(expected.combineN(suiteCount))
          )
        )
      }

    NonEmptyList.fromListUnsafe((1 to 10).toList).reduceMap(testSuites)
  }

  private val pendingSuiteTest =
    pureTest("5 suites: 1 successful, 1 failing, 1 failing/pending, two have some pending assertions") {

      val input = Suite.sequential(
        suite[Id]("All succ") {
          pureTest("Success")(successful |+| successful) |+|
            pureTest("Success 2")(successful)
        },
        suite[Id]("Succ / pending") {
          pureTest("Success")(successful) |+|
            pureTest("Pending")(pending)
        },
        suite[Id]("Failed / pending") {
          pureTest("Failure")(failed) |+|
            pureTest("Pending")(pending)
        },
        suite[Id]("Just pending") {
          pureTest("Pending")(pending)
        },
        suite[Id]("Just failed") {
          pureTest("Failure")(failed)
        }
      )

      ensure(
        input,
        runStats(
          equalTo(
            RunStats(
              suite = Stat(1, 2, 2),
              test = Stat(3, 3, 2),
              assertion = Stat(4, 3, 2)
            )
          )
        )
      )
    }

  val runSuite: Suite[NoEffect] = suite("RunStatsTest") {
    singleSuiteTests |+| multiSuiteTests |+| pendingSuiteTest
  }

}
