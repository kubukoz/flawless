package flawless
import cats.Id
import cats.effect.IO

package object syntax {
  object pure extends Dsl[Id]
  object io   extends Dsl[IO]
}
