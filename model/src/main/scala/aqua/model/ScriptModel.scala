package aqua.model

import aqua.model.func.{FuncCallable, FuncModel}
import cats.Monoid
import cats.data.Chain

/*



val file1 = Model(m: InnerModel, exports: Map[String, ResolvedModel])

import21 = "123"

val file2 = Model(InnerModel(..., file1.exports.get(import21)))
 */

case class ScriptModel(
  models: Chain[Model] = Chain.empty
) extends Model {

  case class Acc(
    arrows: Map[String, FuncCallable] = Map.empty,
    values: Map[String, ValueModel] = Map.empty,
    output: Chain[FuncCallable] = Chain.empty
  )

  lazy val funcs: Chain[FuncModel] = models.collect { case c: FuncModel => c }
  lazy val constants: Chain[ConstantModel] = models.collect { case c: ConstantModel => c }

  lazy val resolveFunctions: Chain[FuncCallable] = models
    .foldLeft(Acc()) {
      case (a, c: ConstantModel) => a.copy(values = a.values.updated(c.name, c.value))
      case (a, func: FuncModel) =>
        val fr = func.capture(a.arrows, a.values)
        a.copy(output = a.output :+ fr, arrows = a.arrows.updated(func.name, fr))
      case (a, _) => a
    }
    .output
}

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
