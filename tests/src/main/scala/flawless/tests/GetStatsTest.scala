package flawless.tests

import flawless.stats._
import cats.implicits._
import cats.Id
import flawless.dsl._
import flawless.predicates.all._
import flawless.SuiteClass
import flawless.data.Suite

object GetStatsTest extends SuiteClass[NoEffect] {
  import RunStats.Stat

  val runSuite: Suite[NoEffect] = suite("GetStatsTest") {
    tests(
      pureTest("1 test: 1 succ / 0 fail") {
        val input = suite[Id]("foo") {
          pureTest("Example") {
            ensure(1, equalTo(1))
          }
        }

        ensureEqual(
          RunStats.fromSuites(List(input)),
          RunStats(
            suite = Stat(1, 1, 0),
            test = Stat(1, 1, 0),
            assertion = Stat(1, 1, 0)
          )
        )
      },
      pureTest("1 test: 1 succ / 1 fail") {
        val input = suite[Id]("foo") {
          pureTest("Example") {
            ensure(1, equalTo(1)) <+> ensure(1, equalTo(2))
          }
        }

        ensureEqual(
          RunStats.fromSuites(List(input)),
          RunStats(
            suite = Stat(1, 0, 1),
            test = Stat(1, 0, 1),
            assertion = Stat(2, 1, 1)
          )
        )
      },
      pureTest("1 test: 1 succ / 2 fail") {
        val input = suite[Id]("foo") {
          pureTest("Example") {
            ensure(1, equalTo(1)) <+> ensure(1, equalTo(2)).combineN(2)
          }
        }

        ensureEqual(
          RunStats.fromSuites(List(input)),
          RunStats(
            suite = Stat(1, 0, 1),
            test = Stat(1, 0, 1),
            assertion = Stat(3, 1, 2)
          )
        )
      }
    )
  }
  //todo property based tests - the total amount of each stat should be the sum of failed+succeeded
}
