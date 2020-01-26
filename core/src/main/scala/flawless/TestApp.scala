package flawless

import flawless.data.Suite
import cats.effect.IOApp
import cats.effect.Sync
import cats.effect.Console
import cats.effect.SyncConsole
import cats.effect.IO

trait TestApp { self: IOApp =>
  implicit val console: Console[IO] = Console.io

  implicit def defaultInterpreter[F[_]: Sync]: Interpreter[F] = {
    implicit val console: Console[F] = SyncConsole.stdio[F]
    implicit val reporter: Reporter[F] = Reporter.consoleInstance[F]

    Interpreter.defaultInterpreter[F]
  }
}

/**
  * Optional trait for you to extend, so that all suites are accessible with the same interface.
  */
trait SuiteClass[+F[_]] {
  def runSuite: Suite[F]
}
