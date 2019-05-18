package flawless.syntax

import cats.Id
import scala.annotation.implicitNotFound

/**
  * Proof that A is F[_] in disguise.
  * Useful when the compiler refuses to infer F[_] = Id.
  *
  * If you're using flawless, and an instance of this can't be found,
  * chances are you're using a type that can't be (partially) unified to the form of F[_] (or, * -> * -kinded).
  * Examples of types that match this (assuming partial unification is turned on, if you're below 2.13):
  *
  * - IO, Eval, Task
  * - ZIO[Env, Nothing, ?]
  * - Kleisli[IO, String, ?]
  *
  */
@implicitNotFound(
  """Couldn't prove that ${A} is some F[${B}]
See Scaladoc of flawless.syntax.Structure for help."""
)
abstract class Structure[A, F[_], B] {
  def convert(a: A): F[B]
}

object Structure {
  implicit def idStructure[A, B](implicit ev: A =:= B): Structure[A, Id, B] =
    fStructure[Id, A, B]

  implicit def fStructure[F[_], A, B](implicit ev: A =:= F[B]): Structure[A, F, B] =
    new Structure[A, F, B] {
      def convert(a: A): F[B] = ev(a)
    }
}
