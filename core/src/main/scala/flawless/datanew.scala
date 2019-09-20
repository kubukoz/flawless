package flawless.data.neu

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Timer
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.implicits._
import cats.Applicative
import cats.Id
import cats.NonEmptyParallel
import cats.Apply
import cats.Parallel
import flawless.data.neu.Suites.Sequence
import flawless.data.neu.Suites.One
import cats.Show
import com.softwaremill.diffx._
import cats.NonEmptyTraverse
import cats.Eval
import cats.effect.Clock
import java.util.concurrent.TimeUnit
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.data.Chain
import cats.Foldable
import cats.kernel.Order
import cats.effect.Console
import cats.effect.ConsoleOut
import cats.effect.SyncConsole
import cats.mtl.ApplicativeLocal
import cats.mtl.instances.all._
import cats.Monad
import cats.FlatMap
import cats.data.Kleisli
import flawless.data.neu.DeepConsole.Depth
import cats.~>
import cats.effect.Resource
import cats.effect.Bracket
import flawless.data.neu.Suites.RResource
import flawless.data.neu.Assertion.Failed
import flawless.data.neu.Assertion.Successful
import flawless.data.neu.TestRun.Pure
import flawless.data.neu.TestRun.Lazy

////////////////////////////////////////////////////////
////////// interpretation stuff ////////////////////////
////////////////////////////////////////////////////////
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
        case One(suite)                                                   => reporter.reportSuite(interpretSuite)(suite).map(One(_))
        case RResource(suites, implicit0(bracket: Bracket[F, Throwable])) => reporter.lift(suites.use(interpret))
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

  def localStateInstance[F[_]: FlatMap](implicit DC: DeepConsole[Kleisli[F, Depth, ?]]): Reporter[F, Kleisli[F, Depth, ?]] =
    consoleInstance(Kleisli.liftK, λ[Kleisli[F, Depth, ?] ~> F](_.run(Depth(0))))
}

////////////////////////////////////////////////////////
////////// testing API /////////////////////////////////
////////////////////////////////////////////////////////

sealed trait Assertion extends Product with Serializable {

  def isSuccessful: Boolean = this match {
    case Failed(_)  => false
    case Successful => true
  }
}

object Assertion {
  case object Successful extends Assertion
  final case class Failed(message: String) extends Assertion
}

sealed trait Suites[F[_]] extends Product with Serializable {
  def interpret(implicit interpreter: Interpreter[F]): F[Suites[Id]] = interpreter.interpret(this)

  /**
    * Modifies every suite in this structure with the given function.
    */
  def via(f: Suite[F] => Suite[F]): Suites[F] = this match {
    case Sequence(suites, traversal)                                 => Sequence(suites.map(_.via(f)), traversal)
    case One(suite)                                                  => One(f(suite))
    case RResource(resuites, implicit0(applicative: Applicative[F])) => RResource(resuites.map(_.via(f)), applicative)
  }

  /**
    * Modifies every test in this structure with the given function.
    */
  def viaTest(f: Test[F] => Test[F]): Suites[F] = via(_.via(f))
}

object Suites {
  def one[F[_]: Applicative](suite: Suite[F]): Suites[F] = One(suite)

  def parallel[F[_]: NonEmptyParallel, G[_]](first: Suites[F], rest: Suites[F]*): Suites[F] =
    Sequence[F](NonEmptyList(first, rest.toList), Traversal.parallel)

  def sequential[F[_]: Apply](first: Suites[F], rest: Suites[F]*): Suites[F] =
    Sequence[F](NonEmptyList(first, rest.toList), Traversal.sequential)

  def resource[F[_]: Bracket[?[_], Throwable]](suitesInResource: Resource[F, Suites[F]]): Suites[F] =
    RResource(suitesInResource, Bracket[F, Throwable])

  final case class Sequence[F[_]](suites: NonEmptyList[Suites[F]], traversal: Traversal[F]) extends Suites[F]

  final case class One[F[_]](suite: Suite[F]) extends Suites[F]
  final case class RResource[F[_]](resuites: Resource[F, Suites[F]], bracket: Bracket[F, Throwable]) extends Suites[F]

  def flatten(suites: Suites[Id]): NonEmptyList[Suite[Id]] = suites match {
    case Sequence(suites, _) => suites.flatMap(flatten)
    case One(suite)          => suite.pure[NonEmptyList]
    case RResource(_, _)     => throw new AssertionError("Impossible")
  }
}

/**
  * An abstraction on methods of combining two effects - parallel or sequential
  */
sealed trait Traversal[F[_]] extends Product with Serializable {
  final def traverse[S[_]: NonEmptyTraverse, A, B](as: S[A])(f: A => F[B]): F[S[B]] = this match {
    case Traversal.Sequential(implicit0(apply: Apply[F]))        => as.nonEmptyTraverse(f)
    case Traversal.Parallel(implicit0(nep: NonEmptyParallel[F])) => Parallel.parNonEmptyTraverse(as)(f)
  }

  final def sequence[S[_]: NonEmptyTraverse, A](as: S[F[A]]): F[S[A]] = traverse(as)(identity)
}

object Traversal {

  final private case class Parallel[F[_]](nep: NonEmptyParallel[F]) extends Traversal[F]
  final private case class Sequential[F[_]](apply: Apply[F]) extends Traversal[F]

  // (potentially specialized) implementation of Sequential for Id,
  // which means identity in case of `sequence` and `map` in case of `traverse`.
  val identity: Traversal[Id] = sequential

  def sequential[F[_]: Apply]: Traversal[F] = Sequential(Apply[F])
  def parallel[F[_]: NonEmptyParallel]: Traversal[F] = Parallel(NonEmptyParallel[F])
}

final case class Suite[F[_]](name: String, tests: NonEmptyList[Test[F]]) {
  def via(f: Test[F] => Test[F]): Suite[F] = Suite(name, tests.map(f))
}

final case class Test[+F[_]](name: String, result: TestRun[F]) {
  //Maybe use via in interpreter?
  //todo add ability for `via` to run effects (think modifyF from monocle)
  def via[G[_]](f: TestRun[F] => TestRun[G]): Test[G] = Test(name, f(result))
}

sealed trait TestRun[+F[_]] extends Product with Serializable {

  //todo signature, changing effects
  def via[F2[a] >: F[a]](f: F[NonEmptyList[Assertion]] => F2[NonEmptyList[Assertion]]): TestRun[F2] = this match {
    case TestRun.Eval(effect)              => TestRun.Eval(f(effect))
    case TestRun.Pure(_) | TestRun.Lazy(_) => this
  }

  def assertions[F2[a] >: F[a]](implicit applicative: Applicative[F2]): F2[NonEmptyList[Assertion]] = this match {
    case TestRun.Eval(effect) => effect
    case TestRun.Pure(result) => result.pure[F2]
    case TestRun.Lazy(result) => result.value.pure[F2]
  }
}

object TestRun {
  final case class Eval[F[_]](effect: F[NonEmptyList[Assertion]]) extends TestRun[F]
  final case class Pure(result: NonEmptyList[Assertion]) extends TestRun[Nothing]
  final case class Lazy(result: cats.Eval[NonEmptyList[Assertion]]) extends TestRun[Nothing]
}

object dsl {
  // Shamelessly ripped off from fs2's Pure type - this is pure genius.
  type NoEffect[A] <: Nothing

  // This name is bad (Predicate implies A => Boolean). Come up with a better name.
  // Possibly worth newtyping.
  // This idea is heavily inspired by ZIO Test.
  type Predicate[-A] = A => Assertion

  def suite[F[_]](name: String)(tests: NonEmptyList[Test[F]]): Suite[F] = new Suite(name, tests)

  def tests[F[_]](firstTest: NonEmptyList[Test[F]], rest: NonEmptyList[Test[F]]*): NonEmptyList[Test[F]] =
    NonEmptyList(firstTest, rest.toList).reduce

  def test[F[_]](name: String)(assertions: F[NonEmptyList[Assertion]]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Eval(assertions)))

  /**
    * Provides access to assertions in a monadic fashion.
    * If no assertions are added, the test completes with a single successful assertion.
    */
  def testMonadic[F[_]: Sync](name: String)(assertions: Assertions[F] => F[Unit]): NonEmptyList[Test[F]] =
    test(name) {
      Ref[F]
        .of(Chain.empty[Assertion])
        .flatMap { ref =>
          assertions(Assertions.refInstance(ref)) *> ref.get
        }
        .map(_.toList.toNel.getOrElse(NonEmptyList.one(Assertion.Successful)))
    }

  def pureTest[F[a] >: NoEffect[a]](name: String)(assertions: NonEmptyList[Assertion]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Pure(assertions)))

  def lazyTest[F[a] >: NoEffect[a]](name: String)(assertions: => NonEmptyList[Assertion]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Lazy(Eval.later(assertions))))

  def assertion(cond: Boolean, ifFalse: String): NonEmptyList[Assertion] = ensure(cond, predicates.all.isTrue(ifFalse))

  def ensure[A](value: A, predicate: Predicate[A]): NonEmptyList[Assertion] = NonEmptyList.one(predicate(value))

  //probably useless
  def failed[F[a] >: NoEffect[a]](name: String): NonEmptyList[Test[F]] = pureTest[F](name)(NonEmptyList.one(Assertion.Failed("Failed")))
}

trait Assertions[F[_]] {
  def add(assertion: Assertion): F[Unit]
  def addAll[S[_]: Foldable](assertions: S[Assertion]): F[Unit]
}

object Assertions {

  def refInstance[F[_]: Applicative](ref: Ref[F, Chain[Assertion]]): Assertions[F] =
    new Assertions[F] {
      def add(assertion: Assertion): F[Unit] = ref.update(_.append(assertion))
      def addAll[S[_]: Foldable](assertions: S[Assertion]): F[Unit] = assertions.traverse_(add)
    }
}

object predicates {

  object all extends DiffInstances {
    import dsl.Predicate

    def greaterThan[A: Order: Show](another: A): Predicate[A] =
      a => if (a > another) Assertion.Successful else Assertion.Failed(show"$a was not greater than $another")

    def equalTo[T: Diff: Show](another: T): Predicate[T] = {
      implicit val showDiff: Show[DiffResult] = _.show

      a =>
        Diff[T].apply(a, another) match {
          case diff if diff.isIdentical => Assertion.Successful
          case diff                     => Assertion.Failed(show"$a was not equal to $another. Diff: $diff")
        }
    }

    val successful: Predicate[Any] = _ => Assertion.Successful
    def failed(message: String): Predicate[Any] = _ => Assertion.Failed(message)

    def isTrue(ifFalse: String): Predicate[Boolean] = {
      case true  => Assertion.Successful
      case false => Assertion.Failed(ifFalse)
    }
  }
}

class NeuExample[F[_]: Timer: Sync] {
  import dsl._
  import predicates.all._
  import scala.concurrent.duration._

  val content = suite("examples") {
    tests(
      pureTest("first test") {
        assertion(1 === 0, "1 was not zero") <+>
          ensure(1, greaterThan(5))
      },
      test("io test") {
        Timer[F].sleep(500.millis).map(a => assertion(a === (()), "unit was not unit"))
      },
      testMonadic[F]("monadic") { implicit assert =>
        val now = Clock[F].monotonic(TimeUnit.MILLISECONDS)
        for {
          before <- now
          _      <- Timer[F].sleep(500.millis)
          after  <- now
          _      <- assert.addAll(ensure(after, greaterThan(before)))
          _      <- assert.addAll(assertion(true, "false"))
        } yield ()
      },
      lazyTest("lazy test") {
        ensure(5, equalTo(4))
      }.combineN(5)
    )
  }
}

// todo better name
trait SuiteClass[F[_]] {
  def runSuite: Suite[F]
}

trait TestApp { self: IOApp =>
  implicit val console: Console[IO] = Console.io

  implicit def defaultInterpreter[F[_]: Sync]: Interpreter[F] = {
    type Effect[A] = Kleisli[F, Depth, A]

    implicit val console: Console[Effect] = SyncConsole.stdio.mapK(Kleisli.liftK)
    implicit val deepConsole: DeepConsole[Effect] = DeepConsole.instance[Effect]

    implicit val reporter: Reporter[F, Effect] = Reporter.localStateInstance[F]

    Interpreter.defaultInterpreter[F, Effect]
  }
}

object Run extends IOApp with TestApp {

  def showResults(testResults: Suites[Id]): String = Suites.flatten(testResults).map(showResult).mkString_("Suites: [\n", "\n", "]")

  def showResult(suite: Suite[Id]): String =
    show"${suite.name}:\n" + suite.tests.map(showR).reduceMap(_.linesIterator.toList.map("  " + _)).mkString("\n")

  def showR(test: Test[Id]): String =
    test.name + ":\n" + (test.result match {
      case TestRun.Pure(result) => result.map(_.toString).map("  " + _).mkString_("\n")
      case _                    => throw new AssertionError("Impossible")
    })

  def run(args: List[String]): IO[ExitCode] = {
    val data = new NeuExample().content

    val tests = Suites.resource {
      Resource.make(IO(println("foo")))(_ => IO(println("closing"))).map { _ =>
        Suites
          .parallel(
            Suites.one(data),
            Suites.sequential(
              Suites.one(data),
              Suites.one(data)
            )
          )
      }
    }

    // deflake -- defined on single test by default
    // customize to apply on whole suites
    // deflake.suites
    // deflake.everything
    //
    // ignore -- defined on assertion by default (ignores assertion it's passed on)
    // ignore(suite) - ignores entire suite straight away
    // ignore(everything) - ignores everything
    // ignore.tests(everything) - ignores each test in everything
    // ignore.assertions(everything)
    // ignore.suite(everything)
    //
    // heuristic - given scope <♾, internal>, can be applied on anything that's > internal.
    // default behavior is for the whole X it's applied on. Can be further scoped up to the point of internal.
    // e.g. aspect defined for scope up to Test, e.g. `caching`:
    // caching(assertion) -- compile error
    // caching(test) -- caches test
    // caching(suite) -- caches whole suite together
    // caching.tests(suite) -- caches each test in suite individually
    // caching.assertions -- doesn't compile
    // caching.tests(everything) -- caches each test in set of suites
    //
    // basically:
    // operator.external(external) == operator(external)
    // operator.internal1(external) == external.via(operator)
    // operator.internal2(external) == external.via(operator.internal1) == external.via(_.via(operator))

    tests.interpret.flatMap { p =>
      IO(println(showResults(p)))
    }
  }.as(ExitCode.Success)
}
