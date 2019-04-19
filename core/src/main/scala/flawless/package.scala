import cats.implicits._

import cats.effect.IO
import cats.effect.ExitCode
import cats.data.Kleisli
import cats.data.NonEmptyList
import cats.data.Reader
import cats.Id

package object flawless {
  type IOTest[A]   = Kleisli[IO, TestRun, A]
  type PureTest[A] = Reader[TestRun, A]

  import cats.effect.Console.io._

  def loadArgs(args: List[String]): IO[TestRun] = IO.pure(TestRun(Nil, Nil))

  def runTests(args: List[String])(iotest: IOTest[NonEmptyList[SpecResult]]) =
    loadArgs(args).flatMap(iotest.flatMap(summarize).run)

  def summarize(fspecs: NonEmptyList[SpecResult]): IOTest[ExitCode] = {
    val weGood = false
    val exit   = if (weGood) ExitCode.Success else ExitCode.Error

    Kleisli.liftF(putStrLn("Test results: xxx failed, xxx succeeded").as(exit))
  }

  object syntax {
    object pure extends Dsl[Id]
    object io extends Dsl[IO]
  }
}
