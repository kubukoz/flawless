package flawless.fixpoint

import cats.~>

final case class HFix[F[_[_], _], A](unfix: F[HFix[F, ?], A]) extends AnyVal

object HFix {

  def hCata[F[_[_], _], G[_], A](hfix: HFix[F, A])(alg: algebra.HAlgebra[F, G])(implicit F: HFunctor[F]): G[A] = {
    val rec = λ[HFix[F, ?] ~> G](hCata(_)(alg))

    alg {
      F.hmap(rec)(hfix.unfix)
    }
  }
}
