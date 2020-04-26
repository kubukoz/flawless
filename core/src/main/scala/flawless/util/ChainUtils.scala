package flawless.util

import scala.annotation.tailrec
import cats.data.Chain
import cats.data.Chain.==:

object ChainUtils {

  def flatReplaceFirst[A](f: PartialFunction[A, Chain[A]]): Chain[A] => Chain[A] = {

    @tailrec
    def go(list: Chain[A], memory: Chain[A]): Chain[A] = list match {
      case Chain.nil => memory
      case head ==: tail =>
        f.lift(head) match {
          case Some(elems) => memory ++ elems ++ tail
          case None        => go(tail, memory append head)
        }
    }

    go(_, Chain.nil)
  }
}
