package aqua.model

import aqua.model.func.FuncModel
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
    case _: FuncModel => true
    case _: ServiceModel => true
    case _: TypeModel => true
    case _: ConstantModel => true
    case _ => false
  }.map(Chain.one).map(ScriptModel(_))
}
