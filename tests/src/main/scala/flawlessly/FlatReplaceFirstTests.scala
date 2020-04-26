package flawlessly

import flawless.SuiteClass
import flawless.NoEffect
import flawless.data.Suite
import cats.data.Chain
import flawless.util.ChainUtils._
import cats.Show
import cats.kernel.Eq
import cats.implicits._

object FlatReplaceFirstTests extends SuiteClass[NoEffect] {
  import flawless.syntax._

  val runSuite: Suite[NoEffect] = suite("FlatReplaceFirstTests") {
    pureTest("if PartialFunction.empty, the function is identity") {
      def challenge[A: Eq: Show](values: Chain[A]) =
        ensureEqualEq(flatReplaceFirst[A](PartialFunction.empty)(values), values)

      challenge(Chain(1, 2, 3, 4)) |+|
        challenge(Chain("a", "b", "c", "d"))
    }
  }
}
