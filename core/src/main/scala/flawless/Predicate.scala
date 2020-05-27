package flawless

import cats.Functor
import cats.implicits._
import flawless.data.Assertion

//Predicate on a value of type A with effects in F.
final case class PredicateT[F[_], -A](private val fun: A => F[Assertion]) extends AnyVal {
  def apply(a: A): F[Assertion] = fun(a)

  def contramap[B](f: B => A): PredicateT[F, B] = PredicateT(fun.compose(f))
}

object PredicateT {
  def const(result: Assertion): Predicate[Any] = liftF[cats.Id](result)
  def liftF[F[_]](resultF: F[Assertion]): PredicateT[F, Any] = PredicateT(_ => resultF)

  implicit class PredicateTExtensions[F[_], A](private val predicate: PredicateT[F, A]) {

    def liftM[G[_]](implicit G: Functor[G], isId: F[Assertion] <:< Assertion): PredicateT[G, G[A]] =
      PredicateT(_.map((predicate.apply _).andThen(isId)))
  }
}
