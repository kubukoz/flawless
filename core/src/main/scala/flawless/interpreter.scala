package flawless

import flawless.data.Suites

import cats.data.NonEmptyList
import cats.implicits._
import cats.Id
import cats.Apply
import flawless.data.Suites.Sequence
import flawless.data.Suites.One
import cats.Show
import cats.effect.ConsoleOut
import cats.mtl.ApplicativeLocal
import cats.mtl.instances.all._
import cats.Monad
import cats.FlatMap
import cats.data.Kleisli
import cats.~>
import flawless.data.Suites.RResource
import flawless.data.Test
import flawless.data.Assertion
import flawless.data.TestRun
import flawless.data.Suite
import flawless.data.Traversal
import flawless.DeepConsole.Depth

trait Interpreter[F[_]] {

  /**
    * Interprets the test structure to the underlying effect. This is where all the actualy execution happens.
    */
  def interpret: Suites[F] => F[Suites[Id]]
}

object Interpreter {
  implicit def defaultInterpreter[F[_]: Monad, G[_]: Apply](implicit reporter: Reporter[F, G]): Interpreter[F] =
    new Interpreter[F] {
      private val interpretTest: Test[F] => F[Test[Id]] = { test =>
        def finish(results: NonEmptyList[Assertion]): Test[Id] = Test(test.name, TestRun.Pure(results))

        test.result match {
          //this is a GADT skolem - you think I'd know what that means by now...
          case eval: TestRun.Eval[f] => eval.effect.map(finish)
          case TestRun.Pure(result)  => finish(result).pure[F]
          case TestRun.Lazy(e)       => e.map(finish).value.pure[F]
        }
      }

      //todo tests in a suite should have multiple methods of traversal
      private val interpretSuite: Suite[F] => G[Suite[Id]] = suite =>
        suite.tests.nonEmptyTraverse(reporter.reportTest(interpretTest.map(reporter.lift(_)))).map(Suite[Id](suite.name, _))

      val interpretN: Suites[F] => G[Suites[Id]] = {
        case Sequence(suites, traversal) =>
          val interpreted = traversal.traverse(suites) {
            //this interpret call will make sure every spec starts with a clean depth scope - watch this space
            interpret
          }

          reporter.lift(interpreted).map(Sequence(_, Traversal.identity))
        case One(suite) => reporter.reportSuite(interpretSuite)(suite).map(One(_))
        case RResource(suites, b) =>
          implicit val bracket = b
          reporter.lift(suites.use(interpret))
      }

      val interpret: Suites[F] => F[Suites[Id]] = interpretN.map(reporter.run(_))
    }
}

trait DeepConsole[F[_]] {
  def putStrLn[A: Show](a: A): F[Unit]
  def nested[A](fa: F[A]): F[A]
}

object DeepConsole {
  def apply[F[_]](implicit F: DeepConsole[F]): DeepConsole[F] = F

  final case class Depth(value: Int) extends AnyVal {
    def deeper: Depth = Depth(value + 1)
  }

  object Depth {
    type Local[F[_]] = ApplicativeLocal[F, Depth]
    def local[F[_]](implicit F: Local[F]): Local[F] = F
  }

  def instance[F[_]: ConsoleOut: Depth.Local: FlatMap]: DeepConsole[F] = new DeepConsole[F] {
    private val spaces = Depth.local[F].ask.map(" " * 2 * _.value)

    def putStrLn[A: Show](a: A): F[Unit] = spaces.flatMap(spacesString => ConsoleOut[F].putStrLn(spacesString + a))

    def nested[A](fa: F[A]): F[A] = Depth.local[F].local(_.deeper)(fa)
  }
}

trait Reporter[F[_], G[_]] {
  //lift the execution effect to the reporting effect
  def lift: F ~> G
  //apply the reporting effect and unlift to execution effect
  def run: G ~> F

  def reportTest: (Test[F] => G[Test[Id]]) => Test[F] => G[Test[Id]]
  def reportSuite: (Suite[F] => G[Suite[Id]]) => Suite[F] => G[Suite[Id]]
}

object Reporter {
  def apply[F[_], G[_]](implicit F: Reporter[F, G]): Reporter[F, G] = F

  def consoleInstance[F[_], G[_]: FlatMap, A](_lift: F ~> G, _unlift: G ~> F)(implicit DC: DeepConsole[G]): Reporter[F, G] =
    new Reporter[F, G] {
      val lift: F ~> G = _lift
      val run: G ~> F = _unlift

      val reportTest: (Test[F] => G[Test[Id]]) => Test[F] => G[Test[Id]] = f =>
        test =>
          //this is going to need access to a summarizer of tests,
          //so that it can display the amount of assertions that succeeded, failed, etc., with colors
          DC.putStrLn("Starting test: " + test.name) *> f(test).flatTap { result =>
            DC.putStrLn("Finished test: " + test.name + s", result: ${result.result}")
          }

      val reportSuite: (Suite[F] => G[Suite[Id]]) => Suite[F] => G[Suite[Id]] = f =>
        suite =>
          DC.putStrLn("Starting suite: " + suite.name) *> DC.nested(f(suite)).flatTap { result =>
            DC.putStrLn("Finished suite: " + suite.name + s", result: ${result.tests}")
          }
    }

  def localStateInstance[F[_]: FlatMap](implicit DC: DeepConsole[Kleisli[F, Depth, *]]): Reporter[F, Kleisli[F, Depth, *]] =
    consoleInstance(Kleisli.liftK, λ[Kleisli[F, Depth, *] ~> F](_.run(Depth(0))))
}
