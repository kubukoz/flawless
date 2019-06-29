package flawless.data.low

import cats.effect.IO
import cats.effect.Resource
import cats.implicits._
import cats.tagless.FunctorK
import cats.Functor
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

  final case class Merge[F[_], Before[_], After[_], A](tests: Before[F[A]], merge: Before[IO[A]] => IO[After[A]], functor: Functor[Before])
    extends TestAlg[F, After[A]]

  final case class Map[F[_], A, B](test: F[A], f: A => B) extends TestAlg[F, B]

  object algebras {

    val interpret: HAlgebra[TestAlg, IO] = new HAlgebra[TestAlg, IO] {

      def apply[A](fa: TestAlg[IO, A]): IO[A] = fa match {
        case Pure(result)           => IO.pure(result)
        case Run(io)                => io
        case m: Map[IO, a, A]       => m.test.map(m.f)
        case LiftResource(r, use)   => r.use(use)
        case Merge(tests, merge, _) => merge(tests)
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
        case Map(test, _) =>
          test.map { r =>
            show"Map($r, <function>)"
          }
        case LiftResource(_, _) => show"LiftResource(<resource>, <function>)".pure[IO]
        case Merge(tests, _, _) => s"Merge($tests, <function>)".pure[IO]
      }
    }
  }

  implicit def testsFunctorK[A]: FunctorK[TestAlg[?[_], A]] = new FunctorK[TestAlg[?[_], A]] {

    def mapK[F[_], G[_]](af: TestAlg[F, A])(fk: F ~> G): TestAlg[G, A] = af match {
      case p: Pure                        => p
      case r: Run                         => r
      case m: Map[F, a, A]                => Map(fk(m.test), m.f)
      case ss: Merge[_, before, after, a] => Merge[G, before, after, a](ss.functor.map(ss.tests)(fk(_)), ss.merge, ss.functor)
      case l: LiftResource[F, a, b]       => LiftResource[G, a, b](l.resource, a => fk(l.f(a)))
    }
  }

  implicit val testsHFunctor: HFunctor[TestAlg] = new HFunctor[TestAlg] {

    def hmap[G[_], I[_]](nt: G ~> I): TestAlg[G, ?] ~> TestAlg[I, ?] = new (TestAlg[G, ?] ~> TestAlg[I, ?]) {
      def apply[A](fa: TestAlg[G, A]): TestAlg[I, A] = testsFunctorK[A].mapK(fa)(nt)
    }
  }
}
