package flawlessly

import flawless._
import flawless.dsl._
import cats.implicits._
import cats.data.State
import cats.effect.kernel.Ref
import cats.Functor
import cats.FlatMap
import cats.Monad

trait MyAlg[F[_]] {
  def reset: F[Unit]
  def hello: F[Int]
}

object MyAlg {
  def apply[F[_]](implicit F: MyAlg[F]): MyAlg[F] = F

  def refInstance[F[_]: Ref.Make: Functor]: F[MyAlg[F]] = Ref[F].of(0).map { ref =>
    new MyAlg[F] {
      val hello: F[Int] = ref.modifyState(
        State.modify[Int](_ + 1) *> State.get
      )

      val reset: F[Unit] = ref.set(0)
    }
  }

}

object TaglessTest {

  def apply[F[_]: MyAlg: FlatMap: Ref.Make]: Suite[F] = suite("tagless") {
    tests(
      test("first tagless")(
        (MyAlg[F].reset *> MyAlg[F].hello, MyAlg[F].hello).mapN { (before, after) =>
          ensureEqual(before, 1) |+| ensureEqual(after, 2)
        }
      ),
      testMonadic[F]("monadic tagless") { implicit assertM =>
        for {
          _      <- MyAlg[F].reset
          before <- MyAlg[F].hello
          _      <- assertM(ensureEqual(before, 1))
          after  <- MyAlg[F].hello
          _      <- assertM(ensureEqual(after, 2))
        } yield ()
      }
    )
  }

}

object TaglessTestLocalResource {

  def apply[F[_]: Ref.Make: Monad]: Suite[F] = Suite.suspend {
    MyAlg.refInstance[F].map { implicit alg =>
      TaglessTest[F].renameEach(_ => "tagless with local instance".pure[F])
    }
  }

}
