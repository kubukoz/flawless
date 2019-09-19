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
import flawless.data.neu.Assertion.Successful
import flawless.data.neu.Assertion.Failed
import cats.Defer
import cats.Eval
import flawless.data.neu.TestRun.Pure

sealed trait Assertion extends Product with Serializable

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
    case Sequence(suites, traversal) => Sequence(suites.map(_.via(f)), traversal)
    case One(suite)                  => One(f(suite))
  }

  /**
    * Modifies every test in this structure with the given function.
    */
  def viaTest(f: Test[F] => Test[F]): Suites[F] = via(_.via(f))
}

trait Interpreter[F[_]] {

  /**
    * Interprets the test structure to the underlying effect. This is where all the actualy execution happens.
    */
  def interpret: Suites[F] => F[Suites[Id]]
}

object Interpreter {
  implicit def applyInterpreter[F[_]: Applicative]: Interpreter[F] = new Interpreter[F] {
    //todo tests in a suite should have multiple methods of traversal
    private val interpretTest: Test[F] => F[Test[Id]] = test => {
      def finish(results: NonEmptyList[Assertion]): Test[Id] = Test(test.name, TestRun.Pure(results))

      test.result match {
        case TestRun.Eval(effect) => effect.map(finish)
        case TestRun.Pure(result) => finish(result).pure[F]
        case TestRun.Lazy(e)      => e.map(finish).value.pure[F]
      }
    }

    private val interpretSuite: Suite[F] => F[Suite[Id]] = suite =>
      suite.tests.nonEmptyTraverse(interpretTest).map(Suite[Id](suite.name, _))

    val interpret: Suites[F] => F[Suites[Id]] = {
      case Sequence(suites, traversal) => traversal.traverse(suites)(interpret).map(Sequence(_, Traversal.identity))
      case One(suite)                  => interpretSuite.apply(suite).map(One(_))
    }
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

object Suites {

  def one[F[_]: Applicative](suite: Suite[F]): Suites[F] = One(suite)

  def parallel[F[_]: NonEmptyParallel, G[_]](first: Suites[F], rest: Suites[F]*): Suites[F] =
    Sequence[F](NonEmptyList(first, rest.toList), Traversal.parallel)

  def sequential[F[_]: Apply](first: Suites[F], rest: Suites[F]*): Suites[F] =
    Sequence[F](NonEmptyList(first, rest.toList), Traversal.sequential)

  final case class Sequence[F[_]](suites: NonEmptyList[Suites[F]], traversal: Traversal[F]) extends Suites[F]

  final case class One[F[_]](suite: Suite[F]) extends Suites[F]
}

final case class Suite[F[_]](name: String, tests: NonEmptyList[Test[F]]) {
  def via(f: Test[F] => Test[F]): Suite[F] = Suite(name, tests.map(f))
}

final case class Test[F[_]](name: String, result: TestRun[F])

sealed trait TestRun[F[_]] extends Product with Serializable

object TestRun {
  final case class Eval[F[_]](effect: F[NonEmptyList[Assertion]]) extends TestRun[F]
  final case class Pure[F[_]](result: NonEmptyList[Assertion]) extends TestRun[F]
  final case class Lazy[F[_]](result: cats.Eval[NonEmptyList[Assertion]]) extends TestRun[F]
}

object dsl {
  //This name is bad (Predicate implies A => Boolean). Come up with a better name.
  //Possibly worth newtyping.
  type Predicate[-A] = A => Assertion

  def suite[F[_]](name: String)(tests: NonEmptyList[Test[F]]): Suite[F] = new Suite(name, tests)

  def tests[F[_]](firstTest: NonEmptyList[Test[F]], rest: NonEmptyList[Test[F]]*): NonEmptyList[Test[F]] =
    NonEmptyList(firstTest, rest.toList).reduce

  def test[F[_]](name: String)(assertions: F[NonEmptyList[Assertion]]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Eval(assertions)))

  //todo add node to test tree to distinguish pure tests
  //todo better type inference? Eval by default?
  def pureTest[F[_]](name: String)(assertions: NonEmptyList[Assertion]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Pure(assertions)))

  def lazyTest[F[_]](name: String)(assertions: => NonEmptyList[Assertion]): NonEmptyList[Test[F]] =
    NonEmptyList.one(Test(name, TestRun.Lazy(Eval.later(assertions))))

  def assertion(cond: Boolean, ifFalse: String): NonEmptyList[Assertion] = ensure(cond, predicates.all.isTrue(ifFalse))

  def ensure[A](value: A, predicate: Predicate[A]): NonEmptyList[Assertion] = NonEmptyList.one(predicate(value))

  def failed[F[_]](name: String): NonEmptyList[Test[F]] = pureTest(name)(NonEmptyList.one(Assertion.Failed("Failed")))
}

object predicates {

  object all extends DiffInstances {
    import dsl.Predicate

    def greaterThan(another: Int): Predicate[Int] =
      a => if (a > another) Assertion.Successful else Assertion.Failed(show"$a was not greater than $another")

    def equalTo[T: Diff: Show](another: T): Predicate[T] = {
      implicit val showDiff: Show[DiffResult] = _.show

      a =>
        Diff[T].apply(a, another) match {
          case diff if diff.isIdentical => Assertion.Successful
          case diff                     => Assertion.Failed(show"$a was not equal to $another. Diff:\n$diff")
        }
    }

    val successful: Predicate[Any] = _ => Assertion.Successful
    def failed(message: String): Predicate[Any] = _ => Assertion.Failed(message)

    def isTrue(ifFalse: String): Predicate[Boolean] = {
      case true  => Successful
      case false => Failed(ifFalse)
    }
  }
}

class NeuExample[F[_]: Timer: Applicative: Defer] {
  import dsl._
  import predicates.all._
  import scala.concurrent.duration._

  val content = suite("examples") {
    tests(
      pureTest[F]("first test") {
        assertion(1 === 0, "1 was not zero") <+>
          ensure(1, greaterThan(5))
      },
      test[F]("io test") {
        Timer[F].sleep(500.millis).map(a => assertion(a === (()), "unit was not unit"))
      },
      lazyTest[F]("lazy test") {
        ensure(5, equalTo(4))
      }.combineN(5)
    )
  }
}

object Run extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val data = new NeuExample().content

    val tests = Suites
      .parallel(
        Suites.one(data),
        Suites.sequential(
          Suites.one(data),
          Suites.one(data)
        )
      )

    tests.viaTest(_ => dsl.failed[IO]("Oh no!").head).interpret.flatMap { p =>
      // IO(println(tests)) *>
      IO(println(p))
    }
  }.as(ExitCode.Success)
}
