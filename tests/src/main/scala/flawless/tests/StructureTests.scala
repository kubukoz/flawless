package flawless.tests

import flawless.Suite
import flawless.SuiteResult
import flawless.IOTest
import cats.effect.IO
import flawless.syntax.Structure
import cats.implicits._

object StructureTests extends Suite {
  import flawless.syntax._

  def function[A, F[_]](a: A)(implicit s: Structure[A, F, Int]): F[Int] = s.convert(a)

  val runSuite: IOTest[SuiteResult] = {
    test("id structure with concrete B") {
      IO(function(5) shouldBe 5)
    } |+| test("IO structure with concrete B") {
      function(IO.pure(5)).map(_ shouldBe 5)
    }
  }
}
