import cats.effect.ExitCode
import cats.implicits._
import cats.effect.ConsoleOut
import flawless.data.Assertion
import flawless.eval.Interpreter
import flawless.eval.summarize
import flawless.eval.toTerminalOutput
import flawless.eval.loadArgs
import cats.Functor

package object flawless {

  import cats.effect.Sync

  import flawless.eval.Reporter

  // Shamelessly ripped off from fs2's Pure type - this is pure genius.
  type NoEffect[A] <: Nothing

  type Suite[+F[_]] = flawless.data.Suite[F]
  val Suite = flawless.data.Suite

  // This name is bad (Predicate implies A => Boolean). Come up with a better name.
  // Possibly worth newtyping.
  // This idea is heavily inspired by ZIO Test.
  final case class PredicateT[F[_], -A](private val fun: A => F[Assertion]) extends AnyVal {
    def apply(a: A): F[Assertion] = fun(a)

    def contramap[B](f: B => A): PredicateT[F, B] = PredicateT(fun.compose(f))
  }

  object PredicateT {
    def const(result: Assertion): Predicate[Any] = liftF[cats.Id](result)
    def liftF[F[_]](resultF: F[Assertion]): PredicateT[F, Any] = PredicateT(_ => resultF)

    implicit class PredicateTExtensions[F[_], A](private val predicate: PredicateT[F, A]) {

      def liftM[G[_]](implicit G: Functor[G], isId: F[Assertion] <:< Assertion): PredicateT[G, G[A]] =
        PredicateT(_.map((predicate.apply _).andThen(isId)))
    }
  }

  type Predicate[A] = PredicateT[cats.Id, A]

  object Predicate {
    def apply[A](f: A => Assertion): Predicate[A] = PredicateT[cats.Id, A](f)
  }

  object syntax extends api.AllDsl with api.AllPredicates
  object dsl extends api.AllDsl
  object predicates extends api.AllPredicates

  def runTests[F[_]: Interpreter: ConsoleOut: Sync](args: List[String])(suites: Suite[F]): F[ExitCode] =
    loadArgs[F](args)
      .flatMap {
        case args if args.visual => Reporter.visual[F]
        case _                   => Reporter.consoleInstance[F]
      }
      .flatMap(Interpreter[F].interpret(_)(suites))
      .map(summarize)
      .map(toTerminalOutput)
      .flatMap(_.leftTraverse(ConsoleOut[F].putStrLn))
      .map(_._2)

}
