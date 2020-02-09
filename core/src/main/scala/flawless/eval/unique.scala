package flawless.eval

import cats.Show

// inspired by christopherdavenport/unique

object unique {

  final class Unique {
    override def toString(): String = "Unique(...)"
    override def equals(obj: Any): Boolean = obj.isInstanceOf[Unique] && (obj.asInstanceOf[Unique] eq this)
  }

  object Unique {
    implicit val show: Show[Unique] = Show.fromToString
  }
}
