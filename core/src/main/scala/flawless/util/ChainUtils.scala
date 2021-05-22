package flawless.util

import cats.data.Chain

import scala.annotation.tailrec

object ChainUtils {

  def flatReplaceFirst[A](f: PartialFunction[A, Chain[A]]): Chain[A] => Chain[A] = {

    @tailrec
    def go(list: Chain[A], memory: Chain[A]): Chain[A] = list.uncons match {
      case None               => memory
      case Some((head, tail)) =>
        f.lift(head) match {
          case Some(elems) => memory ++ elems ++ tail
          case None        => go(tail, memory append head)
        }
    }

    go(_, Chain.nil)
  }

}
