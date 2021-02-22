package flawless

import cats.effect.IOApp
import flawless.eval.Interpreter
import cats.effect.MonadThrow

trait TestApp { self: IOApp =>
  implicit def defaultInterpreter[F[_]: MonadThrow]: Interpreter[F] =
    Interpreter.defaultInterpreter[F]
}

/** Optional trait for you to extend, so that all suites are accessible with the same interface.
  */
trait SuiteClass[+F[_]] {
  def runSuite: Suite[F]
}
