package flawlessly

import flawless.data.Suite
import flawless._
import flawless.syntax._
import cats.implicits._
import flawless.eval.unique.Unique

object HistoryStringifyTests extends SuiteClass[NoEffect] {
  import flawless.eval.Reporter.SuiteHistory
  import SuiteHistory._

  val u = new Unique

  val runSuite: Suite[NoEffect] = suite("HistoryStringifyTests") {
    tests(
      pureTest("stringify on a cell") {
        ensureEqual(Status.Pending.stringify, "▫") |+|
          ensureEqual(Status.Running.stringify, "◼") |+|
          ensureEqual(Status.Succeeded.stringify, "◼") |+|
          ensureEqual(Status.Failed.stringify, "◼")
      },
      pureTest("stringify on a history") {
        val history = SuiteHistory(
          List(
            4 -> Status.Succeeded,
            1 -> Status.Failed,
            1 -> Status.Succeeded,
            4 -> Status.Running,
            1 -> Status.Pending,
            1 -> Status.Succeeded,
            2 -> Status.Pending,
            1 -> Status.Succeeded,
            2 -> Status.Pending
          ).flatMap { case (n, status) => List.fill(n)(status) }.map(Cell(u, _))
        )

        def green(s: String) = Console.GREEN ++ s
        def red(s: String) = Console.RED ++ s
        def yellow(s: String) = Console.YELLOW ++ s
        def clear(s: String) = Console.RESET ++ s

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
            Console.RESET

        ensureEqual(history.stringify, stringified)
      }
    )
  }
}
