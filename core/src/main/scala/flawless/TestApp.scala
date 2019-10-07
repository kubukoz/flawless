package flawless

import flawless.data.Suite
import cats.effect.IOApp
import cats.effect.Sync
import cats.effect.Console
import cats.effect.SyncConsole
import cats.data.Kleisli
import flawless.DeepConsole.Depth
import cats.mtl.instances.all._
import cats.effect.IO

trait TestApp { self: IOApp =>
  implicit val console: Console[IO] = Console.io

  implicit def defaultInterpreter[F[_]: Sync]: Interpreter[F] = {
    type Effect[A] = Kleisli[F, Depth, A]

    implicit val console: Console[Effect] = SyncConsole.stdio[F].mapK(Kleisli.liftK)
    implicit val deepConsole: DeepConsole[Effect] = DeepConsole.instance[Effect]

    implicit val reporter: Reporter[F, Effect] = Reporter.localStateInstance[F]

    Interpreter.defaultInterpreter[F, Effect]
  }
}

// todo better name
trait SuiteClass[+F[_]] {
  def runSuite: Suite[F]
}
