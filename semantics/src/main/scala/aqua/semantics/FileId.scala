package aqua.semantics

import cats.Show
import cats.kernel.Order
import cats.syntax.order.*
import cats.syntax.show.*

trait FileId[I] extends Show[I] with Order[I]

object FileId {
  given FileId[String] with {
    override def show(t: String): String = t

    override def compare(x: String, y: String): Int = x.compare(y)
  }
}


