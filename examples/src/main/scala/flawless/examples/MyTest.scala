package foo

//here goes the demo!

import cats.Applicative
import cats.Id
import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import flawless._
import cats.Eval
import cats.effect.Console.io._
import scala.util.Random
import flawless.examples.doobie.DoobieQueryTests
import doobie.Transactor

object ExampleTest extends IOApp {

  //flaky test detector
  def runUntilFailed(test: IOTest[SuiteResult]): IOTest[SuiteResult] = {
    fs2.Stream
      .repeatEval(test)
      .zipWithIndex
      .dropWhile {
        case (suite, _) => suite.results.forall(_.assertions.value.forall(_.isSuccessful))
      }
      .head
      .evalMap {
        case (failure, successes) =>
        putStrLn(show"Suite failed after ${successes + 1} successes").as(failure)
      }
      .compile
      .lastOrError
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val parallelTests = NonEmptyList.of(
      FirstSuite,
      FirstSuite,
      IOSuite
    )

    val sequentialTests = NonEmptyList.of(
      FirstSuite,
      IOSuite
    )

    val dbTests = {
      val xa = Transactor.fromDriverManager[IO](
        "org.postgresql.Driver",
        "jdbc:postgresql://localhost:5432/postgres",
        "postgres",
        "postgres"
      )
      NonEmptyList.fromListUnsafe(List.fill(10)(new DoobieQueryTests(xa)))
    }

    runTests(args)(
      runUntilFailed(FlakySuite.runSuite).map(NonEmptyList.one) |+|
        NonEmptyList.fromListUnsafe(List.fill(10)(ExpensiveSuite)).parTraverse(_.runSuite) |+| parallelTests
        .parTraverse(
          _.runSuite
        ) |+| sequentialTests.traverse(_.runSuite) |+| dbTests.parTraverse(_.runSuite)
    )
  }
}

object FlakySuite extends Suite {
  import flawless.syntax.io._
  private val flaky = IO(Random.nextInt(10000)).map(_ =!= 0)

  val runSuite: IOTest[SuiteResult] = test("random(10000) =!= 0") {
    flaky.map(_ shouldBe true)
  }
}

object ExpensiveSuite extends Suite {
  import flawless.syntax.eval._

  private def fib(n: Long): Long = {
    def go(a: Long, b: Long, n: Long): Long = {
      if (n > 0)
        go(b, a + b, n - 1)
      else b
    }

    go(1, 1, n - 2)
  }

  val runSuite: IOTest[SuiteResult] = IO.eval {
    tests(
      test("fib(1-8)") {
        NonEmptyList
          .of(
            1L -> 1L,
            2L -> 1L,
            3L -> 2L,
            4L -> 3L,
            5L -> 5L,
            6L -> 8L
          )
          .reduceMap {
            case (x, y) => Eval.later(fib(x)).map(_ shouldBe y)
          }
      },
      test("fib(Int.MaxValue)") {
        Eval.later(fib(Int.MaxValue.toLong)).map(_ shouldBe 6798988702295324957L)
      }
    )
  }
}

object FirstSuite extends Suite {
  val service: MyService[Id] = new MyServiceImpl

  import flawless.syntax.pure._

  override val runSuite: IOTest[SuiteResult] = IO {
    test("job(1) and (2)")(
      service.job(1).shouldBe("I got 1 problems but a test ain't one") |+|
        service.job(2).shouldBe("I got 2 problems but a test ain't one")
    ) |+|
      test("job(1-1000)")(
        NonEmptyList(1, (2 to 1000).toList).reduceMap { n =>
          val result = service.job(n)
          result.shouldBe(show"I got $n problems but a test ain't one") |+|
            result.contains("500").shouldBe(false)
        }
      )
  }
}

object IOSuite extends Suite {

  val service: MyService[IO] = new MyServiceImpl[IO]

  import flawless.syntax.io._

  override val runSuite: IOTest[SuiteResult] = {
    test("job(1) and (2)")(
      service.job(1).map(_.shouldBe("I got 1 problems but a test ain't one")) |+|
        service.job(2).map(_.shouldBe("I got 2 problems but a test ain't one"))
    ) |+|
      test("job(1-1000)")(
        NonEmptyList(1, (2 to 1000).toList).reduceMapM { n =>
          service.job(n).map { result =>
            result.shouldBe(show"I got $n problems but a test ain't one") |+|
              result.contains("500").shouldBe(false)
          }
        }
      )
  }
}

trait MyService[F[_]] {
  def job(i: Int): F[String]
}

class MyServiceImpl[F[_]: Applicative] extends MyService[F] {

  override def job(i: Int): F[String] =
    ("I got " + i + " problems but a test ain't one").pure[F]
}
