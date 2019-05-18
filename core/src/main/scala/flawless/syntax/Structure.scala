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

object Structure extends LowPriority {
  implicit def fStructure[F[_], A, B](implicit ev: A =:= F[B]): Structure[A, F, B] = ev(_)
}

private[syntax] sealed trait LowPriority {

  /**
    * The instance of Structure for F = Id.
    * This instance can be dangerous in case B (the type within F) isn't concrete: types that
    * don't unify nicely will have this instance picked, which may result in surprising behavior (they'll be wrapped in Id no matter what).
    *
    * For this reason, Structure should be used with concrete B types (e.g. `Assertions`, as seen in flawless itself)
    * or with special care (putting type annotations everywhere to be sure, which makes Structure quite useless).
    */
  implicit final def idStructure[A]: Structure[A, Id, A] = a => a
}
