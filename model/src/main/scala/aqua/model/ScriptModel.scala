package aqua.model

import aqua.model.func.{FuncCallable, FuncModel}
import cats.Monoid
import cats.data.Chain

// TODO make one chain to have order
case class ScriptModel(
  models: Chain[Model] = Chain.empty
) extends Model {

  case class Acc(
    arrows: Map[String, FuncCallable],
    values: Map[String, ValueModel]
  )

  lazy val funcs: Chain[FuncModel] = models.collect { case c: FuncModel => c }
  lazy val constants: Chain[ConstantModel] = models.collect { case c: ConstantModel => c }

  def resolveFunctions: Chain[FuncCallable] = {
    val constantsToName =
      constants.map(c => c.name -> c.value).toList.toMap
    funcs
      .foldLeft(
        (
          (
            Map.empty[String, FuncCallable],
            Chain.empty[FuncCallable]
          )
        )
      ) { case ((acc, outputAcc), func) =>
        val fr = func.capture(acc, constantsToName)
        acc -> outputAcc.append(fr)
      }
      ._2
  }
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
  def toScriptPart(m: Model): Option[ScriptModel] = m match {
    case fm: FuncModel => Some(ScriptModel(models = Chain.one(fm)))
    case sm: ServiceModel => Some(ScriptModel(models = Chain.one(sm)))
    case tm: TypeModel => Some(ScriptModel(models = Chain.one(tm)))
    case cm: ConstantModel => Some(ScriptModel(models = Chain.one(cm)))
    case _ => None
  }
}
