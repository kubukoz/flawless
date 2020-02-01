package flawless.eval

import cats.tagless.finalAlg
import cats.effect.Sync
import flawless.eval.unique.Unique
import cats.Show

// inspired by christopherdavenport/unique

object unique {

  sealed trait Unique {
    override def toString(): String = "Unique(...)"
  }

  object Unique {
    implicit val show: Show[Unique] = Show.fromToString
  }
}

@finalAlg
trait UniqueFactory[F[_]] {
  def get: F[Unique]
}

object UniqueFactory {

  implicit def syncFactory[F[_]: Sync]: UniqueFactory[F] = new UniqueFactory[F] {
    val get: F[Unique] = Sync[F].delay(new Unique {})
  }
}
