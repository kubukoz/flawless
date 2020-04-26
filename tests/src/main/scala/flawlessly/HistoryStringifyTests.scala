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

  def fakeHistory(statuses: (Int, Status)*) =
    SuiteHistory(
      Chain
        .fromSeq(statuses)
        .flatMap {
          case (n, status) => Chain.fromSeq(List.fill(n)(status))
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
          4 -> Status.Succeeded,
          1 -> Status.Failed,
          1 -> Status.Succeeded,
          4 -> Status.Running,
          1 -> Status.Pending,
          1 -> Status.Succeeded,
          2 -> Status.Pending,
          1 -> Status.Succeeded,
          2 -> Status.Pending
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
          2 -> Status.Pending,
          2 -> Status.Succeeded
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
