package flawless.examples.doobie

import flawless.Suite
import flawless.SuiteResult
import flawless.IOTest
import doobie.Transactor
import cats.effect.IO
import cats.implicits._

class DoobieQueryTests(xa: Transactor[IO]) extends Suite {
  import flawless.syntax.io._

  val runSuite: IOTest[SuiteResult] = {
    import doobie.implicits._
    
    test("select 1") {
      sql"select 1".query[Int].to[List].transact(xa).map(_ shouldBe List(1))
    }
  }
}
