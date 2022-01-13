package aqua.raw

import cats.Monoid
import cats.data.Chain
import aqua.raw.arrow.FuncRaw

case class ContextRaw(
                       parts: Chain[Raw] = Chain.empty
                     ) extends Raw

object ContextRaw {
  implicit object CRMonoid extends Monoid[ContextRaw] {
    override def empty: ContextRaw = ContextRaw()

    override def combine(x: ContextRaw, y: ContextRaw): ContextRaw =
      ContextRaw(
        x.parts ++ y.parts
      )
  }

  def contextPart(raw: Raw): ContextRaw = raw match {
    case cr: ContextRaw => cr
    case _ =>
      ContextRaw(Chain.one(raw).filter {
        case _: FuncRaw => true
        case _: ServiceRaw => true
        case _: TypeRaw => true
        case _: ConstantRaw => true
        case _ => false
      })
  }
}
