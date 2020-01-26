package flawless.tests

import flawless.SuiteClass
import flawless.data.Suite
import flawless.dsl._
import cats.tagless.finalAlg
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.data.State
import cats.FlatMap

@finalAlg
trait MyAlg[F[_]] {
  def reset: F[Unit]
  def hello: F[Int]
}

object MyAlg {

  def syncInstance[F[_]: Sync]: F[MyAlg[F]] = Ref[F].of(0).map { ref =>
    new MyAlg[F] {
      val hello: F[Int] = ref.modifyState(
        State.modify[Int](_ + 1) *> State.get
      )

      val reset: F[Unit] = ref.set(0)
    }
  }
}

final class TaglessTest[F[_]: MyAlg: Sync] extends SuiteClass[F] {

  val runSuite: Suite[F] = suite("tagless") {
    tests(
      test("first tagless")(
        (MyAlg[F].reset *> MyAlg[F].hello, MyAlg[F].hello).mapN { (before, after) =>
          ensureEqual(before, 1) |+| ensureEqual(after, 2)
        }
      ),
      testMonadic[F]("monadic tagless") { implicit assertions =>
        for {
          _      <- MyAlg[F].reset
          before <- MyAlg[F].hello
          _      <- assertions.addAll(ensureEqual(before, 1))
          after  <- MyAlg[F].hello
          _      <- assertions.addAll(ensureEqual(after, 2))
        } yield ()
      }
    )
  }
}
