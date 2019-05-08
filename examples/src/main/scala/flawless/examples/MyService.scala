package flawless.examples

import cats.Applicative
import cats.implicits._

trait MyService[F[_]] {
  def job(i: Int): F[String]
}

object MyService {
  def instance[F[_]: Applicative]: MyService[F] = i => ("I got " + i + " problems but a test ain't one").pure[F]
}
