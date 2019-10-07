package flawless.examples.doobie

import flawless.data._
import flawless.data.dsl._
import doobie.Transactor
import cats.effect.IO
import cats.implicits._
import flawless.SuiteClass

final class DoobieQueryTests(xa: Transactor[IO]) extends SuiteClass[IO] {

  val runSuite: Suite[IO] = suite("DoobieQueryTests") {
    import doobie.implicits._

    test("select 1") {
      sql"select 1".query[Int].to[List].transact(xa).map(ensureEqual(_, List(1)))
    }
  }
}
