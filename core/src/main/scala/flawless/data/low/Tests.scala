package flawless.data.low

import cats.effect.IO
import cats.effect.Resource
import cats.implicits._
import cats.tagless.FunctorK
import cats.Functor
import cats.Semigroup
import cats.~>
import flawless.SuiteResult
import flawless.TTest
import flawless.fixpoint.HFix
import flawless.fixpoint.HFunctor
import flawless.fixpoint.algebra
import flawless.fixpoint.algebra.HAlgebra

sealed trait Tests[+F[_], A] extends Product with Serializable

object Tests {
  type HFixed[A] = HFix[Tests, A]

  final case class Pure(result: SuiteResult) extends Tests[Nothing, SuiteResult]
  final case class Run(iotest: IO[SuiteResult]) extends Tests[Nothing, SuiteResult]
  final case class Both[F[_], A](left: F[A], right: F[A], sem: Semigroup[A]) extends Tests[F, A]
  final case class LiftResource[F[_], A, B](resource: Resource[IO, A], f: A => F[B]) extends Tests[F, B]
  final case class Sequence[F[_], S[_], A](tests: S[F[A]], merge: S[IO[A]] => IO[S[A]], functor: Functor[S]) extends Tests[F, S[A]]

  object algebras {

    val interpret: HAlgebra[Tests, IO] = new HAlgebra[Tests, IO] {

      def apply[A](fa: Tests[IO, A]): IO[A] = fa match {
        case Pure(result)                                  => IO.pure(result)
        case Run(io)                                       => io
        case Both(left, right, implicit0(s: Semigroup[A])) => left |+| right
        case LiftResource(r, use)                          => r.use(use)
        case Sequence(tests, merge, _)                     => merge(tests)
      }
    }

    def visitRun(mod: IO[SuiteResult] => IO[SuiteResult]): HAlgebra[Tests, TTest] =
      new algebra.HAlgebra[Tests, TTest] {

        def apply[A](fa: Tests[TTest, A]): TTest[A] = fa match {
          case Run(io) => new TTest(HFix[Tests, A](Run(mod(io))))
          case e       => new TTest(HFix[Tests, A](testsFunctorK[A].mapK(e)(λ[TTest ~> HFixed](_.tree))))
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
}
