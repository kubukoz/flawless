package flawless

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import cats.effect.Console.io._
import scala.concurrent.duration._
import scala.util.Random

// //test runner for the whole module
// object FlawlessTests extends IOApp with TestApp {

//   def run(args: List[String]): IO[ExitCode] = runTests[IO](args) {
//     Suites.sequential(
//       GetStatsTest.runSuite.toSuites,
//       ReporterTest.runSuite.toSuites
//     )
//   }
// }
object Demo extends IOApp {
  val max = 100

  val files =
    List(
      "ApplicativeTests",
      "MonadTests",
      "BusinessLogicSuite",
      "GeneratorTests",
      "MagicTests",
      "AbstractSingletonProxyFactoryBeanTests"
    )

  def putClear(txt: String): IO[Unit] =
    putStrLn(s"\033[2J\033[H$header\n" + txt.linesIterator.mkString("\n"))

  val results = List(
    30 -> true,
    20 -> false,
    20 -> true,
    10 -> false,
    20 -> true
  )

  def progress(n: Int): IO[Unit] = {
    val testName = files(n % files.size)
    val done = "◼"
    val failed = "◼"
    val notdone = "▫"

    val dur = IO(Random.nextInt(20 - (n min 19))).map(_ * 0.8).map(3 + _).map(_.millis)

    val currentString = (n < 100).guard[Option].as(Console.BLUE + done).combineAll

    val remainingString = Console.RESET + (notdone * (max - n - currentString.nonEmpty.guard[List].size))

    val completedResults = results.flatMap { case (count, b) => List.fill(count)(b) }.take(n)

    val squares =
      completedResults.foldMap {
        case true  => Console.GREEN + done
        case false => Console.RED + failed
      } + currentString + remainingString

    val failureString =
      if (completedResults.contains(false)) s"${Console.RED}[${completedResults.count(!_)} failed]${Console.RESET} "
      else ""

    val noise = IO(Random.nextBoolean()).flatMap {
      case true => IO(println("some random noise\nmore random noise\neven more random noise"))
      case _    => IO.unit
    }

    noise *>
      putClear(s"""$squares
                  |$n/100 suites ($n%) $failureString($testName)""".stripMargin) <*
      dur.flatMap(IO.sleep)
  }

  val showFailures = {

    val failedNames =
      (
        results.flatMap { case (count, b) => List.fill(count)(b) },
        Stream.continually(files).flatten.take(100).toList
      ).parTupled.collect {
        case (false, s) => s
      }

    putError("Failed suites: " + failedNames.mkString("\n"))
  }

  val header = "Flawless 0.0.1 👌"

  def run(args: List[String]): IO[ExitCode] = {
    putStrLn(s"$header\n\nInitializing...") *>
      IO.sleep(500.millis) *>
      (1 to max).toList.traverse_(progress) *>
      showFailures *>
      putStrLn("All tests finished")
  } as ExitCode.Success
}
