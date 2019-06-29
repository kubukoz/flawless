package flawless.fixpoint

import cats.~>

trait HFunctor[F[_[_], _]] {
  def hmap[G[_], I[_]](nt: G ~> I): F[G, ?] ~> F[I, ?]
}
