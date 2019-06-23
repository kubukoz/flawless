package flawless

import cats.implicits._
import cats.effect.IO
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import flawless.stats.Location
import cats.Parallel
import cats.NonEmptyTraverse
import cats.Applicative
import cats.Functor
import cats.effect.Resource
import cats.NonEmptyParallel
import cats.tagless.FunctorK
import cats.tagless.Derive
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.~>

sealed trait Tests[+F[_], A] {
  // def interpret: IO[A]
  // def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[A]
  // final def liftA[F[_]: Applicative]: Tests[F[A]] = this.map(_.pure[F])
}

case class HFix[F[_[_], _], A](unfix: F[HFix[F, ?], A])
import cats.effect.Console.io._

object TestsExample extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    import flawless.syntax._
    val t = Tests.sequence {
      test("foo") {
        IO(1).map(_ shouldBe 1)
      }.combineN(3).pure[NonEmptyList]
    }

    val r: IO[NonEmptyList[SuiteResult]] = Tests.hCata(Tests.interpret, t)

    r.map(_.toString).flatMap(putStrLn(_)).as(ExitCode.Success)
  }
}

object Tests {
  type TTest[A] = HFix[Tests, A]

  import structure._

  type JustString[A] = String

  val interpret: HAlgebra[Tests, IO] = new HAlgebra[Tests, IO] {

    def apply[A](fa: Tests[IO, A]): IO[A] = fa match {
      case Run(io)                                       => io
      case Both(left, right, implicit0(s: Semigroup[A])) => left |+| right
      case LiftResource(r, use)                          => r.use(use)
      case Sequence(tests, merge, _)                     => merge(tests)
    }
  }

  val show: HAlgebra[Tests, JustString] = new HAlgebra[Tests, JustString] {

    def apply[A](fa: Tests[JustString, A]): String = fa match {
      case Run(_)                => "iotest"
      case Both(left, right, _)  => s"Both($left |+| $right)"
      case LiftResource(_, _)    => s"LiftResource(<resource>, <function>)"
      case Sequence(tests, _, _) => s"Sequence($tests, <function>)"
    }
  }

  implicit def functorkTests[A]: FunctorK[Tests[?[_], A]] = new FunctorK[Tests[?[_], A]] {

    def mapK[F[_], G[_]](af: Tests[F, A])(fk: F ~> G): Tests[G, A] = af match {
      case Run(iotest)              => new Run(iotest) {}
      case b: Both[F, a]            => new Both[G, a](fk(b.left), fk(b.right), b.sem) {}
      case ss: Sequence[_, s, a]    => new Sequence[G, s, a](ss.functor.map(ss.tests)(fk(_)), ss.merge, ss.functor) {}
      case l: LiftResource[_, a, b] => new LiftResource[G, a, b](l.resource, a => fk(l.f(a))) {}
    }
  }

  type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G

  trait HFunctor[F[_[_], _]] {
    def hmap[G[_], I[_]](nt: G ~> I): F[G, ?] ~> F[I, ?]
  }

  implicit val testsHFunctor: HFunctor[Tests] = new HFunctor[Tests] {

    def hmap[G[_], I[_]](nt: G ~> I): Tests[G, ?] ~> Tests[I, ?] = new (Tests[G, ?] ~> Tests[I, ?]) {
      def apply[A](fa: Tests[G, A]): Tests[I, A] = functorkTests[A].mapK(fa)(nt)
    }
  }

  def hCata[F[_[_], _], G[_], I](alg: HAlgebra[F, G], hfix: HFix[F, I])(implicit F: HFunctor[F]): G[I] = {
    val inner = hfix.unfix
    val nt = F.hmap(
      new (HFix[F, ?] ~> G) {
        def apply[A](fa: HFix[F, A]): G[A] = hCata(alg, fa)
      }
    )(inner)

    alg(nt)
  }

  def liftIO(result: IO[SuiteResult]): TTest[SuiteResult] = HFix[Tests, SuiteResult](new Run(result) {})
  // def liftResource[A, B](tests: Resource[IO, A])(f: A => Tests[B]): Tests[B] = new LiftResource(tests, f) {}

  // def parSequence[F[_]: NonEmptyTraverse](
  //   suites: F[Tests[SuiteResult]]
  // )(implicit nep: NonEmptyParallel[IO, IO.Par]
  // ): Tests[F[SuiteResult]] =
  //   new Sequence[F, SuiteResult](suites, Parallel.parNonEmptySequence(_)) {}

  def sequence[S[_]: NonEmptyTraverse, A](suites: S[TTest[A]]): TTest[S[A]] =
    HFix(new Sequence[TTest, S, A](suites, _.nonEmptySequence, Functor[S]) {})

  private[flawless] object structure {
    sealed abstract case class Run(iotest: IO[SuiteResult]) extends Tests[Nothing, SuiteResult] {
      // def interpret: IO[SuiteResult] = iotest
      // def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[SuiteResult] = new Run(v(iotest)) {}
    }

    sealed abstract case class LiftResource[F[_], A, B](resource: Resource[IO, A], f: A => F[B]) extends Tests[F, B] {
      // def interpret: IO[B] = resource.use(f(_).interpret)
      // def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[B] = new LiftResource[A, B](resource, f(_).visit(v)) {}
    }

    sealed abstract case class Sequence[F[_], S[_], A](tests: S[F[A]], merge: S[IO[A]] => IO[S[A]], functor: Functor[S])
      extends Tests[F, S[A]] {

      // def interpret: IO[F[A]] =
      //   merge(tests.map(_.interpret))

      // def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[F[A]] = new Sequence(tests.map(_.visit(v)), merge) {}
    }

    sealed abstract case class Both[F[_], A](left: F[A], right: F[A], sem: Semigroup[A]) extends Tests[F, A] {
      // def interpret: IO[A] = (left.interpret |+| right.interpret)
      // def visit(v: IO[SuiteResult] => IO[SuiteResult]): Tests[A] = new Both(left.visit(v), right.visit(v)) {}
    }
  }

  implicit def semigroup[F[_], A](implicit F: Semigroup[A]): Semigroup[TTest[A]] =
    (a, b) => HFix[Tests, A](new Both(a, b, F) {})
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
