package flawlessly

import flawless.data.Suite
import flawless._
import flawless.syntax._
import cats.implicits._
import flawless.eval.unique.Unique
import cats.data.Chain

object HistoryStringifyTests extends SuiteClass[NoEffect] {
  import flawless.eval.Reporter.SuiteHistory
  import SuiteHistory._

  val u = new Unique

  def green(s: String) = Console.GREEN ++ s
  def red(s: String) = Console.RED ++ s
  def yellow(s: String) = Console.YELLOW ++ s
  def clear(s: String) = Console.RESET ++ s

  def fakeHistory(statuses: Status.type => (Int, Status)*): SuiteHistory =
    SuiteHistory(
      Chain
        .fromSeq(statuses.map(_(Status)))
        .flatMap { case (n, status) =>
          Chain.fromSeq(List.fill(n)(status))
        }
        .map(Cell(u, _))
    )

  val runSuite: Suite[NoEffect] = suite("HistoryStringifyTests") {
    tests(
      pureTest("stringify on a cell") {
        ensureEqual(Status.Pending.stringify, "▫") |+|
          ensureEqual(Status.Running.stringify, "◼") |+|
          ensureEqual(Status.Succeeded.stringify, "◼") |+|
          ensureEqual(Status.Failed.stringify, "◼")
      },
      pureTest("stringify on a history") {
        val history = fakeHistory(
          4 -> _.Succeeded,
          1 -> _.Failed,
          1 -> _.Succeeded,
          4 -> _.Running,
          1 -> _.Pending,
          1 -> _.Succeeded,
          2 -> _.Pending,
          1 -> _.Succeeded,
          2 -> _.Pending
        )

        val stringified =
          Console.RESET ++
            green("◼◼◼◼") ++
            red("◼") ++
            green("◼") ++
            yellow("◼◼◼◼") ++
            clear("▫") ++
            green("◼") ++
            clear("▫▫") ++
            green("◼") ++
            clear("▫▫") ++
            "\n" ++
            red("[1 failed]") ++
            Console.RESET

        ensureEqual(history.stringify, stringified)
      },
      pureTest("stringify without failures") {
        val history = fakeHistory(
          2 -> _.Pending,
          2 -> _.Succeeded
        )

        val stringified =
          Console.RESET ++
            clear("▫▫") ++
            green("◼◼") ++
            Console.RESET

        ensureEqual(history.stringify, stringified)
      }
    )
  }

}
