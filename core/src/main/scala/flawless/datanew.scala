package flawless.data.neu

import cats.data.NonEmptyList
import cats.effect.IO
import cats.Eval
import cats.effect.Timer
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.~>
import cats.implicits._
import cats.Applicative
import cats.Id
import cats.NonEmptyParallel
import cats.Apply
import cats.Parallel
import flawless.data.neu.Suites.Sequence
import flawless.data.neu.Suites.One

sealed trait Assertion extends Product with Serializable

object Assertion {
  case object Successful extends Assertion
  final case class Failed(message: String) extends Assertion
}

object types {
  type Comp[F[_], G[_]] = { type L[A] = F[G[A]] }
}

import types._
sealed trait Suites[F[_]] extends Product with Serializable {
  // def visit(f: Suite[F] => Suite[F]): Suites[F] = ??? // Suites(suites.map(f), sequence)

  def interpret(implicit F: Applicative[F]): F[NonEmptyList[Suite[Id]]] = this match {
    case Sequence(suites, sequence) => sequence(suites.map(_.interpret)).map(_.flatten)
    case One(suite) =>
      suite.tests.nonEmptyTraverse(a => a.result.tupleLeft(a.name)).map { tests =>
        Suite(suite.name, tests.map((Test.apply[Id] _).tupled)).pure[NonEmptyList]
      }
  }

}

object Suites {

  def lift[F[_]: Applicative](suite: Suite[F]): Suites[F] = One(suite)

  def parallel[F[_]: Apply, G[_]](first: Suites[F], rest: Suites[F]*)(implicit nep: NonEmptyParallel[F]): Suites[F] =
    Sequence[F](NonEmptyList(first, rest.toList), λ[(NonEmptyList Comp F)#L ~> (F Comp NonEmptyList)#L](Parallel.parNonEmptySequence(_)))

  def sequential[F[_]: Apply](first: Suites[F], rest: Suites[F]*): Suites[F] =
    Sequence[F](NonEmptyList(first, rest.toList), λ[(NonEmptyList Comp F)#L ~> (F Comp NonEmptyList)#L](_.nonEmptySequence))

  final case class Sequence[F[_]](suites: NonEmptyList[Suites[F]], sequence: (NonEmptyList Comp F)#L ~> (F Comp NonEmptyList)#L)
    extends Suites[F]

  final case class One[F[_]](suite: Suite[F]) extends Suites[F]

}

final case class Suite[F[_]](name: String, tests: NonEmptyList[Test[F]]) {

  def visit(f: F[NonEmptyList[Assertion]] => F[NonEmptyList[Assertion]]): Suite[F] =
    Suite(name, tests.map(test => Test(test.name, f(test.result))))
}

final case class Test[F[_]](name: String, result: F[NonEmptyList[Assertion]])

object dsl {
  type Predicate[A] = A => Assertion

  def suite[F[_]](name: String)(tests: NonEmptyList[Test[F]]): Suite[F] = new Suite(name, tests)

  def tests[F[_]](firstTest: NonEmptyList[Test[F]], rest: NonEmptyList[Test[F]]*): NonEmptyList[Test[F]] =
    NonEmptyList(firstTest, rest.toList).reduce

  def test(name: String)(assertions: IO[NonEmptyList[Assertion]]): NonEmptyList[Test[IO]] = NonEmptyList.one(Test(name, assertions))

  def pureTest(name: String)(assertions: NonEmptyList[Assertion]): NonEmptyList[Test[IO]] =
    NonEmptyList.one(Test(name, IO.pure(assertions)))

  def lazyTest(name: String)(assertions: => NonEmptyList[Assertion]): NonEmptyList[Test[IO]] =
    NonEmptyList.one(Test(name, IO.eval(Eval.later(assertions))))

  def assertion(cond: Boolean, ifFalse: String): NonEmptyList[Assertion] = NonEmptyList.one {
    if (cond) Assertion.Successful
    else Assertion.Failed(ifFalse)
  }

  def ensure[A](value: A, predicate: Predicate[A]): NonEmptyList[Assertion] = NonEmptyList.one(predicate(value))
}

object predicates {

  object all {
    import dsl.Predicate

    def greaterThan(another: Int): Predicate[Int] =
      a => if (a > another) Assertion.Successful else Assertion.Failed(show"$a was not greater than $another")
  }
}

class NeuExample(implicit timer: Timer[IO]) {
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
        IO.sleep(500.millis).map(a => assertion(a === (()), "unit was not unit"))
      },
      lazyTest("lazy test") {
        assertion(false == false, "false was not false").combineN(2)
      }
    )
  }
}

object Run extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val data = new NeuExample().content

    Suites
      .parallel(
        Suites.lift(data),
        Suites.sequential(
          Suites.lift(data),
          Suites.lift(data)
        )
      )
      .interpret
      .flatMap { p =>
        IO(println(p))
      }
  }.as(ExitCode.Success)
}
