package flawless.data.low

import cats.effect.IO
import cats.effect.Resource
import cats.implicits._
import cats.tagless.FunctorK
import cats.Functor
import cats.Semigroup
import cats.data.NonEmptyChain
import cats.~>
import flawless.SuiteResult
import flawless.Tests
import flawless.fixpoint.HFix
import flawless.fixpoint.HFunctor
import flawless.fixpoint.algebra
import flawless.fixpoint.algebra.HAlgebra

sealed trait TestAlg[+F[_], A] extends Product with Serializable

object TestAlg {
  type HFixed[A] = HFix[TestAlg, A]

  final case class Pure(result: SuiteResult) extends TestAlg[Nothing, SuiteResult]
  final case class Run(iotest: IO[SuiteResult]) extends TestAlg[Nothing, SuiteResult]
  final case class LiftResource[F[_], A, B](resource: Resource[IO, A], f: A => F[B]) extends TestAlg[F, B]
  //todo merge these two? add new type parameter to sequence and rename it to merge
  final case class Merge[F[_], A](tests: NonEmptyChain[F[A]], sem: Semigroup[A]) extends TestAlg[F, A]
  final case class Sequence[F[_], S[_], A](tests: S[F[A]], merge: S[IO[A]] => IO[S[A]], functor: Functor[S]) extends TestAlg[F, S[A]]
  final case class Map[F[_], A, B](test: F[A], f: A => B) extends TestAlg[F, B]

  object algebras {

    val interpret: HAlgebra[TestAlg, IO] = new HAlgebra[TestAlg, IO] {

      def apply[A](fa: TestAlg[IO, A]): IO[A] = fa match {
        case Pure(result)                             => IO.pure(result)
        case Run(io)                                  => io
        case m: Map[IO, a, A]                         => m.test.map(m.f)
        case Merge(tests, implicit0(s: Semigroup[A])) => tests.reduce
        case LiftResource(r, use)                     => r.use(use)
        case Sequence(tests, merge, _)                => merge(tests)
      }
    }

    def visitRun(mod: IO[SuiteResult] => IO[SuiteResult]): HAlgebra[TestAlg, Tests] =
      new algebra.HAlgebra[TestAlg, Tests] {

        def apply[A](fa: TestAlg[Tests, A]): Tests[A] = fa match {
          case Run(io) => new Tests(HFix[TestAlg, A](Run(mod(io))))
          case e       => new Tests(HFix[TestAlg, A](testsFunctorK[A].mapK(e)(λ[Tests ~> HFixed](_.tree))))
        }
      }

    type IOString[_] = IO[String]

    val show: algebra.HAlgebra[TestAlg, IOString] = new algebra.HAlgebra[TestAlg, IOString] {

      def apply[A](fa: TestAlg[IOString, A]): IO[String] = fa match {
        case Pure(result) => show"result($result)".pure[IO]
        case Run(test)    => test.map(result => show"Run($result)")
        case Merge(elems, _) =>
          (elems: NonEmptyChain[IO[String]]).sequence.map { results =>
            results.mkString_("Merge(", " |+| ", ")")
          }
        case Map(test, _) =>
          test.map { r =>
            show"Map($r, <function>)"
          }
        case LiftResource(_, _)    => show"LiftResource(<resource>, <function>)".pure[IO]
        case Sequence(tests, _, _) => s"Sequence($tests, <function>)".pure[IO]
      }
    }
  }

  implicit def testsFunctorK[A]: FunctorK[TestAlg[?[_], A]] = new FunctorK[TestAlg[?[_], A]] {

    def mapK[F[_], G[_]](af: TestAlg[F, A])(fk: F ~> G): TestAlg[G, A] = af match {
      case p: Pure                  => p
      case r: Run                   => r
      case m: Map[F, a, A]          => Map(fk(m.test), m.f)
      case m: Merge[F, a]           => Merge[G, a](m.tests.map(fk(_)), m.sem)
      case ss: Sequence[_, s, a]    => Sequence[G, s, a](ss.functor.map(ss.tests)(fk(_)), ss.merge, ss.functor)
      case l: LiftResource[F, a, b] => LiftResource[G, a, b](l.resource, a => fk(l.f(a)))
    }
  }

  implicit val testsHFunctor: HFunctor[TestAlg] = new HFunctor[TestAlg] {

    def hmap[G[_], I[_]](nt: G ~> I): TestAlg[G, ?] ~> TestAlg[I, ?] = new (TestAlg[G, ?] ~> TestAlg[I, ?]) {
      def apply[A](fa: TestAlg[G, A]): TestAlg[I, A] = testsFunctorK[A].mapK(fa)(nt)
    }
  }
}
