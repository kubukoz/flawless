package flawlessly

import flawless.data.Suite
import flawless.syntax._
import flawless._
import cats.implicits._
import cats.data.Chain
import flawless.eval.Reporter
import flawless.eval.Interpreter
import cats.Show
import cats.effect.Sync
import cats.Foldable
import cats.data.NonEmptyList
import cats.Parallel
import cats.mtl.MonadState
import cats.mtl.FunctorTell
import cats.mtl.instances.all._
import cats.Alternative
import cats.Monad
import cats.data.ReaderWriterStateT
import cats.effect.Bracket

object TestReporter {
  implicit def instance[F[_]: Sync]: TestReporter[F] = new TestReporter[F]
}

final class TestReporter[F[_]: Sync] {
  sealed trait LogEvent extends Product with Serializable

  object LogEvent {
    case class ReplaceWith(parent: Int, childCount: Int) extends LogEvent
    case class Report(event: Reporter.Event[Int]) extends LogEvent
    implicit val show: Show[LogEvent] = Show.fromToString
  }

  type WC[A] = ReaderWriterStateT[F, Unit, Chain[LogEvent], Int, A]

  implicit val wcBracket: Bracket[WC, Throwable] = Sync.catsReaderWriteStateTSync
  //The instance shall not be used for parallelism! It's pretty much just a marker
  implicit val wcParallel: Parallel[WC] = Parallel.identity

  val reporter: Reporter.Aux[WC, Int] = {
    def make[
      M[_]: MonadState[*[_], Int]: Monad: FunctorTell[*[_], S[LogEvent]],
      S[_]: Alternative
    ]: Reporter.Aux[M, Int] =
      new Reporter[M] {
        type Identifier = Int
        val root: Int = 0

        private val logger = FunctorTell[M, S[LogEvent]]

        private val ident: M[Identifier] = MonadState[M, Int].modify(_ + 1) *> MonadState[M, Int].get

        def splitParent(parent: Int, count: Int): M[NonEmptyList[Int]] =
          logger.tell((LogEvent.ReplaceWith(parent, count): LogEvent).pure[S]) *> ident
            .replicateA(count)
            .map(NonEmptyList.fromListUnsafe)

        def publish(event: Reporter.Event[Identifier]): M[Unit] =
          logger.tell((LogEvent.Report(event): LogEvent).pure[S])
      }

    make[WC, Chain]
  }

  val interpreter: Interpreter[WC] = Interpreter.defaultInterpreter[WC]

  def reported[G[_]: Foldable](expectedWritten: G[LogEvent]): PredicateT[F, Suite[WC]] =
    reportedWith[G](equalTo(expectedWritten.toList))

  //todo naming
  def reportedWith[G[_]: Foldable](writtenPredicate: Predicate[List[LogEvent]]): PredicateT[F, Suite[WC]] =
    select((a: Suite[WC]) => interpreter.interpret(reporter)(a).written.runA((), 0).map(_.toList))(
      writtenPredicate.liftM[F]
    )

}
