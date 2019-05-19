package flawless.tests

import flawless.Suite
import flawless.SuiteResult
import flawless.IOTest
import cats.effect.IO
import flawless.syntax.Structure
import cats.implicits._
import cats.Functor
import cats.Eval

object StructureTests extends Suite {
  import flawless.syntax._

  def function[A, F[_]](a: A)(implicit s: Structure[A, Int, F]): F[Int] =
    s.convert(a)

  def functionAbstract[A, F[_], B](
    a: A
  )(implicit s: Structure[A, B, F], f: Functor[F]): F[(B, B)] =
    s.convert(a).fproduct(identity)

  val runSuite: IOTest[SuiteResult] =
    tests(
      test("id structure with concrete B") {
        IO {
          function(5) shouldBe 5
        }
      },
      test("IO structure with concrete B") {
        function(IO.pure(5)).map(_ shouldBe 5)
      },
      test("id structure with abstract B") {
        IO {
          functionAbstract(5) shouldBe ((5, 5))
        }
      },
      test("IO structure with abstract B") {
        functionAbstract(5.pure[IO]).map(_ shouldBe ((5, 5)))
      },
      test("weird structure with abstract B") {
        functionAbstract(5.pure[Eval].pure[IO]).map {
          _.tupled.value shouldBe ((5, 5))
        }
      }
    )
}
