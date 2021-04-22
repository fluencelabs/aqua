package aqua.model

import aqua.model.func.{FuncCallable, FuncModel}
import cats.Monoid
import cats.data.Chain

// TODO make one chain to have order
case class ScriptModel(
  funcs: Chain[FuncModel] = Chain.empty,
  services: Chain[ServiceModel] = Chain.empty,
  types: Chain[TypeModel] = Chain.empty,
  constants: Chain[ConstantModel] = Chain.empty
) extends Model {

  case class Acc(
    arrows: Map[String, FuncCallable],
    values: Map[String, ValueModel]
  )

  def resolveFunctions: Chain[FuncCallable] = {
    val constantsToName = constants.map(c => c.name -> c.value).toList.toMap
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
        x.funcs ++ y.funcs,
        x.services ++ y.services,
        x.types ++ y.types,
        x.constants ++ y.constants
      )
  }

  // Builds a ScriptModel if given model can be considered as a part of a script
  def toScriptPart(m: Model): Option[ScriptModel] = m match {
    case fm: FuncModel => Some(ScriptModel(funcs = Chain.one(fm)))
    case sm: ServiceModel => Some(ScriptModel(services = Chain.one(sm)))
    case tm: TypeModel => Some(ScriptModel(types = Chain.one(tm)))
    case cm: ConstantModel => Some(ScriptModel(constants = Chain.one(cm)))
    case _ => None
  }
}
