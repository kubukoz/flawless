package flawless.examples.doobie

import flawless.data.neu._
import flawless.data.neu.dsl._
import doobie.Transactor
import cats.effect.IO
import cats.implicits._

final class DoobieQueryTests(xa: Transactor[IO]) extends SuiteClass[IO] {

  val runSuite: Suite[IO] = suite("DoobieQueryTests") {
    import doobie.implicits._

    test("select 1") {
      sql"select 1".query[Int].to[List].transact(xa).map(ensureEqual(_, List(1)))
    }
  }
}
