package aqua.raw

import aqua.raw.RawPart.contextPart
import aqua.raw.ops.{FuncOp, RawTag}

import cats.Semigroup
import cats.syntax.semigroup.*

trait Raw

object Raw {

  def error(log: String): Raw = Empty(log)
  def empty(log: String): Raw = Empty(log)

  case class Empty(log: String) extends Raw

  given Semigroup[Raw] with {

    override def combine(x: Raw, y: Raw): Raw =
      (x, y) match {
        case (l: FuncOp, r: FuncOp) => FuncOp(l.tree |+| r.tree)

        case (l: Empty, r: Empty) => Empty(l.log + " |+| " + r.log)
        case (_: Empty, r) => r
        case (l, _: Empty) => l

        case (l, r) => contextPart(l) |+| contextPart(r)
      }
  }
}
