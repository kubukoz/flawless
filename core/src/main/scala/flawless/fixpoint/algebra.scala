package flawless.fixpoint

import cats.~>

object algebra {
  type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G
}
