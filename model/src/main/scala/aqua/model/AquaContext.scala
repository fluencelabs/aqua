package aqua.model

import aqua.model.func.body.{CallServiceTag, FuncOp}
import aqua.model.func.{ArgsCall, FuncCallable, FuncModel}
import aqua.types.Type
import cats.Monoid
import cats.syntax.apply._
import cats.syntax.functor._

case class AquaContext(
  funcs: Map[String, FuncCallable],
  types: Map[String, Type],
  values: Map[String, ValueModel],
  abilities: Map[String, AquaContext],
  // TODO: merge this with abilities, when have ability resolution variance
  services: Map[String, ServiceModel]
) {

  def allTypes(prefix: String = ""): Map[String, Type] =
    abilities
      .foldLeft(types) { case (ts, (k, v)) =>
        ts ++ v.allTypes(k + ".")
      }
      .map(_.swap.map(prefix + _).swap)

  def allFuncs(prefix: String = ""): Map[String, FuncCallable] =
    abilities
      .foldLeft(funcs) { case (ts, (k, v)) =>
        ts ++ v.allFuncs(k + ".")
      }
      .map(_.swap.map(prefix + _).swap)

  def allValues(prefix: String = ""): Map[String, ValueModel] =
    abilities
      .foldLeft(values) { case (ts, (k, v)) =>
        ts ++ v.allValues(k + ".")
      }
      .map(_.swap.map(prefix + _).swap)

  def allServices(prefix: String = ""): Map[String, ServiceModel] =
    abilities
      .foldLeft(services) { case (ts, (k, v)) =>
        ts ++ v.allServices(k + ".")
      }
      .map(_.swap.map(prefix + _).swap)
}

object AquaContext {

  implicit object AquaContextMonoid extends Monoid[AquaContext] {

    override def empty: AquaContext =
      AquaContext(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

    override def combine(x: AquaContext, y: AquaContext): AquaContext =
      AquaContext(
        x.funcs ++ y.funcs,
        x.types ++ y.types,
        x.values ++ y.values,
        x.abilities ++ y.abilities,
        x.services ++ y.services
      )
  }

  def fromServiceModel(sm: ServiceModel, serviceId: ValueModel): AquaContext =
    AquaContext(
      funcs = sm.arrows.toSortedMap.map { case (fnName, arrowType) =>
        val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
        fnName ->
          FuncCallable(
            fnName,
            // TODO: capture ability resolution, get ID from the call context
            FuncOp.leaf(CallServiceTag(sm.defaultId.getOrElse(serviceId), fnName, call, None)),
            args,
            (ret.map(_.model), arrowType.res).mapN(_ -> _),
            Map.empty,
            Map.empty
          )
      },
      types = Map.empty,
      values = Map.empty,
      abilities = Map.empty,
      services = Map.empty
    )

  def fromScriptModel(sm: ScriptModel, init: AquaContext = Monoid.empty[AquaContext]): AquaContext =
    sm.models.foldLeft(init) {
      case (ctx, c: ConstantModel) =>
        ctx.copy(values =
          if (c.allowOverrides && ctx.values.contains(c.name)) ctx.values
          else ctx.values.updated(c.name, c.value.resolveWith(ctx.values))
        )
      case (ctx, func: FuncModel) =>
        val fr = func.capture(ctx.funcs, ctx.values)
        ctx.copy(funcs = ctx.funcs.updated(func.name, fr))
      case (ctx, t: TypeModel) =>
        ctx.copy(types = ctx.types.updated(t.name, t.`type`))
      case (ctx, m: ServiceModel) =>
        ctx.copy(
          abilities = m.defaultId.fold(ctx.abilities)(id =>
            ctx.abilities.updated(m.name, fromServiceModel(m, id))
          ),
          services = ctx.services.updated(m.name, m)
        )
      case (ctx, _) => ctx
    }
}
