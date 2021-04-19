package aqua.model

import aqua.model.func.{FuncCallable, FuncModel}
import cats.Monoid
import cats.data.Chain

case class ScriptModel(
  funcs: Chain[FuncModel] = Chain.empty,
  services: Chain[ServiceModel] = Chain.empty,
  types: Chain[TypeModel] = Chain.empty
) extends Model {

  def enqueue(m: Model): ScriptModel = m match {
    case f: FuncModel => copy(funcs.append(f))
    case s: ServiceModel => copy(services = services.append(s))
    case t: TypeModel => copy(types = types.append(t))
    case _ => this
  }

  def resolveFunctions: Chain[FuncCallable] =
    funcs
      .foldLeft((Map.empty[String, FuncCallable], Chain.empty[FuncCallable])) {
        case ((funcsAcc, outputAcc), func) =>
          val fr = func.captureArrows(funcsAcc)
          funcsAcc.updated(func.name, fr) -> outputAcc.append(fr)
      }
      ._2

}

object ScriptModel {

  implicit object SMMonoid extends Monoid[ScriptModel] {
    override def empty: ScriptModel = ScriptModel()

    override def combine(x: ScriptModel, y: ScriptModel): ScriptModel =
      ScriptModel(x.funcs ++ y.funcs, x.services ++ y.services, x.types ++ y.types)
  }

  def toScriptPart(m: Model): Option[ScriptModel] = m match {
    case fm: FuncModel => Some(ScriptModel(funcs = Chain.one(fm)))
    case sm: ServiceModel => Some(ScriptModel(services = Chain.one(sm)))
    case tm: TypeModel => Some(ScriptModel(types = Chain.one(tm)))
    case _ => None
  }
}
