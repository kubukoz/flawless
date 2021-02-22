package flawlessly

import flawless.SuiteClass
import flawless.NoEffect
import flawless.data.Suite
import cats.data.Chain
import cats.data.NonEmptyChain
import flawless.util.ChainUtils._
import cats.Show
import cats.kernel.Eq
import cats.implicits._

object FlatReplaceFirstTests extends SuiteClass[NoEffect] {
  import flawless.syntax._

  val runSuite: Suite[NoEffect] = suite("FlatReplaceFirstTests") {
    tests(
      pureTest("if PartialFunction.empty, the function is identity") {
        def challenge[A: Eq: Show](values: Chain[A]) =
          ensureEqualEq(flatReplaceFirst[A](PartialFunction.empty)(values), values)

        challenge(Chain(1, 2, 3, 4)) |+|
          challenge(Chain("a", "b", "c", "d"))
      },
      pureTest("if the function matches all elements, the head is changed") {
        def challenge[A: Eq: Show](values: NonEmptyChain[A], replacement: Chain[A]) =
          ensureEqualEq(flatReplaceFirst[A] { case _ => replacement }(values.toChain), replacement ++ values.tail)

        challenge(NonEmptyChain(1, 2, 3), Chain(10, 20)) |+|
          challenge(NonEmptyChain(2, 3), Chain(10, 20)) |+|
          challenge(NonEmptyChain(3), Chain(10, 20))
      },
      pureTest("if the function matches one of the unique elements, others are left unchanged") {
        ensureEqualEq(flatReplaceFirst[Int] { case 1 => Chain(10, 20) }(Chain(1, 2, 3)), Chain(10, 20, 2, 3)) |+|
          ensureEqualEq(flatReplaceFirst[Int] { case 2 => Chain(10, 20) }(Chain(1, 2, 3)), Chain(1, 10, 20, 3)) |+|
          ensureEqualEq(flatReplaceFirst[Int] { case 3 => Chain(10, 20) }(Chain(1, 2, 3)), Chain(1, 2, 10, 20))
      },
      pureTest("if the function matches more than one element, only the first one is changed") {
        ensureEqualEq(flatReplaceFirst[Int] { case 1 => Chain(10, 20) }(Chain(1, 2, 1, 3)), Chain(10, 20, 2, 1, 3))
      }
    )
  }

}
