package aqua.raw

import aqua.raw.ops.FuncOp
import cats.Semigroup

trait Raw

object Raw {

  def error(log: String): Raw = Empty(log)
  def empty(log: String): Raw = Empty(log)

  case class Empty(log: String) extends Raw

  implicit object MergeRaw extends Semigroup[Raw] {

    import ContextRaw.CRMonoid
    import FuncOp.FuncOpSemigroup
    import ContextRaw.contextPart

    override def combine(x: Raw, y: Raw): Raw =
      (x, y) match {
        case (l: FuncOp, r: FuncOp) =>
          FuncOpSemigroup.combine(l, r)
        case (l: ContextRaw, r: ContextRaw) =>
          CRMonoid.combine(l, r)

        case (l: Empty, r: Empty) => Empty(l.log + " |+| " + r.log)
        case (_: Empty, r) => r
        case (l, _: Empty) => l

        case (l, r) =>
          CRMonoid.combine(
            contextPart(l),
            contextPart(r)
          )

      }
  }
}
