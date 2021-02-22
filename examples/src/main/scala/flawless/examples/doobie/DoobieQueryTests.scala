// package flawless.examples.doobie

// import flawless.data._
// import flawless.dsl._
// import doobie.Transactor
// import cats.implicits._
// import flawless.SuiteClass
// import cats.effect.Bracket

// final class DoobieQueryTests[F[_]: Bracket[*[_], Throwable]](xa: Transactor[F]) extends SuiteClass[F] {

//   val runSuite: Suite[F] = suite("DoobieQueryTests") {
//     import doobie.implicits._

//     test("select 1") {
//       sql"select 1".query[Int].to[List].map(ensureEqual(_, List(1))).transact(xa)
//     }
//   }

// }
