package flawless

import cats.effect.IOApp
import cats.effect.Sync
import cats.effect.Console
import flawless.eval.Interpreter
import cats.effect.SyncConsole

trait TestApp { self: IOApp =>
  implicit def stdioConsole[F[_]: Sync]: Console[F] = SyncConsole.stdio

  implicit def defaultInterpreter[F[_]: Sync]: Interpreter[F] =
    Interpreter.defaultInterpreter[F]
}

/** Optional trait for you to extend, so that all suites are accessible with the same interface.
  */
trait SuiteClass[+F[_]] {
  def runSuite: Suite[F]
}
