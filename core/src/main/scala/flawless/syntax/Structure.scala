package flawless.syntax

import flawless.Assertions
import cats.Id

/**
  * Proof that A is F[_] in disguise.
  * Useful when the compiler refuses to infer F[_] = Id.
  */
trait Structure[A, F[_]] {
  def convert(a: A): F[Assertions]
}

object Structure {
  implicit val idStructure: Structure[Assertions, Id]        = fStructure[Id]
  implicit def fStructure[F[_]]: Structure[F[Assertions], F] = a => a
}
