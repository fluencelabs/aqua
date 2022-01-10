package aqua.model

import aqua.raw.arrow.FuncRaw
import aqua.raw.{ConstantRaw, ServiceRaw, TypeRaw}
import cats.Monoid
import cats.data.Chain

case class ScriptModel(
  models: Chain[Model] = Chain.empty
) extends Model

object ScriptModel {

  implicit object SMMonoid extends Monoid[ScriptModel] {
    override def empty: ScriptModel = ScriptModel()

    override def combine(x: ScriptModel, y: ScriptModel): ScriptModel =
      ScriptModel(
        x.models ++ y.models
      )
  }

  // Builds a ScriptModel if given model can be considered as a part of a script
  def toScriptPart(m: Model): Option[ScriptModel] = Option(m).filter {
    case _: FuncRaw => true
    case _: ServiceRaw => true
    case _: TypeRaw => true
    case _: ConstantRaw => true
    case _ => false
  }.map(Chain.one).map(ScriptModel(_))
}
