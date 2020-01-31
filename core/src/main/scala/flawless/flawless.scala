import cats.effect.ExitCode
import cats.implicits._
import cats.effect.ConsoleOut
import flawless.data.Assertion
import flawless.eval.Interpreter
import flawless.eval.summarize
import flawless.eval.loadArgs

package object flawless {

  import cats.effect.concurrent.Ref

  import cats.effect.Sync

  import flawless.eval.Reporter

  // Shamelessly ripped off from fs2's Pure type - this is pure genius.
  type NoEffect[A] <: Nothing

  type Suite[+F[_]] = flawless.data.Suite[F]
  val Suite = flawless.data.Suite

  // This name is bad (Predicate implies A => Boolean). Come up with a better name.
  // Possibly worth newtyping.
  // This idea is heavily inspired by ZIO Test.
  type Predicate[-A] = A => Assertion

  object syntax extends api.AllDsl with api.AllPredicates
  object dsl extends api.AllDsl
  object predicates extends api.AllPredicates

  def runTests[F[_]: Interpreter: ConsoleOut: Sync](args: List[String])(suites: Suite[F]): F[ExitCode] =
    loadArgs[F](args).flatMap { args =>
      Ref[F].of(Reporter.SuiteHistory.initial).flatMap { ref =>
        import com.olegpy.meow.effects._
        val reporter = ref.runState { implicit MS =>
          if (args.visual) Reporter.visual[F] else Reporter.consoleInstance[F]
        }

        Interpreter[F].interpret(reporter)(suites).flatMap(summarize[F])
      }
    }
}
