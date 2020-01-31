package flawless

import cats.effect.IOApp
import cats.effect.Sync
import cats.effect.Console
import cats.effect.IO
import flawless.eval.Interpreter

trait TestApp { self: IOApp =>
  implicit val console: Console[IO] = Console.io

  implicit def defaultInterpreter[F[_]: Sync]: Interpreter[F] =
    Interpreter.defaultInterpreter[F]
}

/**
  * Optional trait for you to extend, so that all suites are accessible with the same interface.
  */
trait SuiteClass[+F[_]] {
  def runSuite: Suite[F]
}
