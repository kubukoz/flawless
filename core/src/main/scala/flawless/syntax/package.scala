package flawless
import cats.Id
import cats.effect.IO
import cats.Eval

package object syntax {
  object pure extends Dsl[Id]
  object eval extends Dsl[Eval]
  object io   extends Dsl[IO]
}
