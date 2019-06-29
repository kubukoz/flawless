package flawless

import cats.implicits._
import cats.effect.IO
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import flawless.stats.Location
import cats.Parallel
import cats.NonEmptyTraverse
import cats.Functor
import cats.effect.Resource
import cats.NonEmptyParallel
import cats.tagless.{Derive, FunctorK}
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.~>

sealed trait Tests[+F[_], A] extends Product with Serializable

case class HFix[F[_[_], _], A](unfix: F[HFix[F, ?], A]) {

  def hCata[G[_]](alg: algebra.HAlgebra[F, G])(implicit F: HFunctor[F]): G[A] = {
    val rec = λ[HFix[F, ?] ~> G](_.hCata(alg))

    alg {
      F.hmap(rec)(this.unfix)
    }
  }
}

object algebra {
  type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G
}

trait HFunctor[F[_[_], _]] {
  def hmap[G[_], I[_]](nt: G ~> I): F[G, ?] ~> F[I, ?]
}

import cats.effect.Console.io._

object TestsExample extends IOApp {

  import flawless.syntax._

  val tests = Tests.sequence {
    test("foo") {
      IO(1).map(_ shouldBe 1)
    }.combineN(3).pure[NonEmptyList]
  }

  val prog = putStrLn("Interpreting tests. Text:") *>
    putStrLn(tests.debug) *>
    tests.interpret

  def run(args: List[String]): IO[ExitCode] = prog.map(_.toString).flatMap(putStrLn).as(ExitCode.Success)
}

object Tests {
  type TTest[A] = HFix[Tests, A]

  import structure._

  implicit class TestsExtensions[A](private val tests: TTest[A]) extends AnyVal {
    def interpret: IO[A] = tests.hCata(Tests.algebras.interpret)
    def visit(v: IO[SuiteResult] => IO[SuiteResult]): TTest[A] = tests.hCata(Tests.algebras.visitRun(v))
    def debug: String = tests.hCata(Tests.algebras.show)
  }

  object algebras {

    val interpret: algebra.HAlgebra[Tests, IO] = new algebra.HAlgebra[Tests, IO] {

      def apply[A](fa: Tests[IO, A]): IO[A] = fa match {
        case Pure(result)                                  => IO.pure(result)
        case Run(io)                                       => io
        case Both(left, right, implicit0(s: Semigroup[A])) => left |+| right
        case LiftResource(r, use)                          => r.use(use)
        case Sequence(tests, merge, _)                     => merge(tests)
      }
    }

    def visitRun(mod: IO[SuiteResult] => IO[SuiteResult]): algebra.HAlgebra[Tests, TTest] =
      new algebra.HAlgebra[Tests, TTest] {

        def apply[A](fa: Tests[TTest, A]): TTest[A] = fa match {
          case Run(io) => HFix[Tests, A](Run(mod(io)))
          case e       => HFix(e)
        }
      }

    type JustString[A] = String

    val show: algebra.HAlgebra[Tests, JustString] = new algebra.HAlgebra[Tests, JustString] {

      def apply[A](fa: Tests[JustString, A]): String = fa match {
        case Pure(result)          => s"result($result)"
        case Run(_)                => "iotest"
        case Both(left, right, _)  => s"Both($left |+| $right)"
        case LiftResource(_, _)    => s"LiftResource(<resource>, <function>)"
        case Sequence(tests, _, _) => s"Sequence($tests, <function>)"
      }
    }
  }

  implicit def testsFunctorK[A]: FunctorK[Tests[?[_], A]] = new FunctorK[Tests[?[_], A]] {

    def mapK[F[_], G[_]](af: Tests[F, A])(fk: F ~> G): Tests[G, A] = af match {
      case Pure(result)             => Pure(result)
      case Run(iotest)              => Run(iotest)
      case b: Both[F, a]            => Both[G, a](fk(b.left), fk(b.right), b.sem)
      case ss: Sequence[_, s, a]    => Sequence[G, s, a](ss.functor.map(ss.tests)(fk(_)), ss.merge, ss.functor)
      case l: LiftResource[_, a, b] => LiftResource[G, a, b](l.resource, a => fk(l.f(a)))
    }
  }

  implicit val testsHFunctor: HFunctor[Tests] = new HFunctor[Tests] {

    def hmap[G[_], I[_]](nt: G ~> I): Tests[G, ?] ~> Tests[I, ?] = new (Tests[G, ?] ~> Tests[I, ?]) {
      def apply[A](fa: Tests[G, A]): Tests[I, A] = testsFunctorK[A].mapK(fa)(nt)
    }
  }

  def liftIO(result: IO[SuiteResult]): TTest[SuiteResult] = HFix[Tests, SuiteResult](Run(result))
  def liftResource[A, B](tests: Resource[IO, A])(f: A => TTest[B]): TTest[B] = HFix(LiftResource(tests, f))

  def parSequence[S[_]: NonEmptyTraverse, A](suites: S[TTest[A]])(implicit nep: NonEmptyParallel[IO, IO.Par]): TTest[S[A]] =
    HFix(Sequence[TTest, S, A](suites, Parallel.parNonEmptySequence(_), Functor[S]))

  def sequence[S[_]: NonEmptyTraverse, A](suites: S[TTest[A]]): TTest[S[A]] =
    HFix(Sequence[TTest, S, A](suites, _.nonEmptySequence, Functor[S]))

  private[flawless] object structure {
    final case class Pure(result: SuiteResult) extends Tests[Nothing, SuiteResult]
    final case class Run(iotest: IO[SuiteResult]) extends Tests[Nothing, SuiteResult]
    final case class Both[F[_], A](left: F[A], right: F[A], sem: Semigroup[A]) extends Tests[F, A]
    final case class LiftResource[F[_], A, B](resource: Resource[IO, A], f: A => F[B]) extends Tests[F, B]
    final case class Sequence[F[_], S[_], A](tests: S[F[A]], merge: S[IO[A]] => IO[S[A]], functor: Functor[S]) extends Tests[F, S[A]]
  }

  implicit def semigroup[F[_], A](implicit F: Semigroup[A]): Semigroup[TTest[A]] =
    (a, b) => HFix[Tests, A](Both(a, b, F))
}

final case class AssertionFailure(text: String, location: Location)

final case class Assertions(value: NonEmptyList[Assertion])

object Assertions {
  implicit val semigroup: Semigroup[Assertions] = (a, b) => Assertions(a.value |+| b.value)
}

sealed trait Assertion extends Product with Serializable {
  def isFailed: Boolean = fold(false, _ => true)
  def isSuccessful: Boolean = !isFailed

  def fold[A](successful: => A, failed: AssertionFailure => A): A = this match {
    case Assertion.Successful       => successful
    case Assertion.Failed(failures) => failed(failures)
  }
}

object Assertion {
  case object Successful extends Assertion
  final case class Failed(failure: AssertionFailure) extends Assertion
}

final case class TestResult(name: String, assertions: Assertions) {
  def isFailed: Boolean = assertions.value.exists(_.isFailed)
  def isSuccessful: Boolean = !isFailed
}

final case class SuiteResult(results: NonEmptyList[TestResult]) extends AnyVal {
  def isFailed: Boolean = results.exists(_.isFailed)
  def isSuccessful: Boolean = !isFailed
}

object SuiteResult {
  implicit val semigroup: Semigroup[SuiteResult] = (a, b) => SuiteResult(a.results |+| b.results)
}

trait Suite { self =>
  def runSuite: Tests.TTest[SuiteResult]
}
