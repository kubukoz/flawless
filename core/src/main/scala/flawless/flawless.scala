import cats.effect.ExitCode

import cats.implicits._
import cats.effect.std.Console
import flawless.data.Assertion
import flawless.eval.Interpreter
import flawless.eval.summarize
import flawless.eval.toTerminalOutput
import flawless.eval.loadArgs
import cats.Monad
import cats.effect.kernel.Unique
import cats.effect.kernel.Ref

package object flawless {

  import flawless.eval.Reporter

  // Shamelessly ripped off from fs2's Pure type - this is pure genius.
  type NoEffect[A] <: Nothing

  type Suite[+F[_]] = flawless.data.Suite[F]
  val Suite = flawless.data.Suite

  type Predicate[A] = PredicateT[cats.Id, A]

  object Predicate {
    def apply[A](f: A => Assertion): Predicate[A] = PredicateT[cats.Id, A](f)
  }

  object syntax extends api.AllDsl with api.AllPredicates
  object dsl extends api.AllDsl
  object predicates extends api.AllPredicates

  def runTests[F[_]: Interpreter: Console: Unique: Ref.Make: Monad](args: List[String])(suites: Suite[F]): F[ExitCode] =
    loadArgs[F](args)
      .flatMap {
        case args if args.visual => Reporter.visual[F]
        case _                   => Reporter.consoleInstance[F]
      }
      .flatMap(Interpreter[F].interpret(_)(suites))
      .map(summarize)
      .map(toTerminalOutput)
      .flatMap(_.leftTraverse(Console[F].println(_)))
      .map(_._2)

}
