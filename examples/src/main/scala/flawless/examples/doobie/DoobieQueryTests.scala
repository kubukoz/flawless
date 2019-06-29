package flawless.examples.doobie

import flawless.Suite
import flawless.SuiteResult
import doobie.Transactor
import cats.effect.IO
import cats.implicits._
import flawless.Tests

final class DoobieQueryTests(xa: Transactor[IO]) extends Suite {
  import flawless.syntax._

  val runSuite: Tests[SuiteResult] = {
    import doobie.implicits._

    test("select 1") {
      sql"select 1".query[Int].to[List].transact(xa).map(_ shouldBe List(1))
    }
  }
}
