package aqua.model

import aqua.model.func.raw.{CallServiceTag, FuncOp}
import aqua.model.func.{ArgsCall, FuncCallable, FuncModel}
import aqua.types.{StructType, Type}
import cats.Monoid
import cats.data.NonEmptyMap
import cats.syntax.functor.*
import cats.syntax.monoid.*
import scribe.Logging

import scala.collection.immutable.SortedMap

case class AquaContext(
  module: Option[String],
  declares: Set[String],
  exports: Option[AquaContext],
  funcs: Map[String, FuncCallable],
  types: Map[String, Type],
  values: Map[String, ValueModel],
  abilities: Map[String, AquaContext],
  // TODO: merge this with abilities, when have ability resolution variance
  services: Map[String, ServiceModel]
) {

  private def prefixFirst[T](prefix: String, pair: (String, T)): (String, T) =
    (prefix + pair._1, pair._2)

  def pick(
    name: String,
    rename: Option[String],
    declared: Boolean = module.nonEmpty
  ): Option[AquaContext] =
    Option
      .when(!declared || declares(name)) {
        val targetName = rename.getOrElse(name)
        def getter[T](g: AquaContext => Map[String, T]): Map[String, T] =
          g(this).get(name).map(targetName -> _).map(Map(_)).getOrElse(Map.empty)
        AquaContext.blank.copy(
          funcs = getter(_.funcs),
          types = getter(_.types),
          values = getter(_.values),
          abilities = getter(_.abilities),
          services = getter(_.services)
        )
      }
      .filter(_.`type`(name).nonEmpty)

  def allTypes(prefix: String = ""): Map[String, Type] =
    abilities
      .foldLeft(types) { case (ts, (k, v)) =>
        ts ++ v.allTypes(k + ".")
      }
      .map(prefixFirst(prefix, _))

  def allFuncs(prefix: String = ""): Map[String, FuncCallable] =
    abilities
      .foldLeft(funcs) { case (ts, (k, v)) =>
        ts ++ v.allFuncs(k + ".")
      }
      .map(prefixFirst(prefix, _))

  def allValues(prefix: String = ""): Map[String, ValueModel] =
    abilities
      .foldLeft(values) { case (ts, (k, v)) =>
        ts ++ v.allValues(k + ".")
      }
      .map(prefixFirst(prefix, _))

  def allServices(prefix: String = ""): Map[String, ServiceModel] =
    abilities
      .foldLeft(services) { case (ts, (k, v)) =>
        ts ++ v.allServices(k + ".")
      }
      .map(prefixFirst(prefix, _))

  def `type`(name: String): Option[StructType] =
    NonEmptyMap
      .fromMap(
        SortedMap.from(
          funcs.view.mapValues(_.arrowType) ++
            types ++
            values.view.mapValues(_.lastType) ++
            services.view.mapValues(_.`type`) ++
            abilities.flatMap { case (n, c) =>
              c.`type`(n).map(n -> _)
            }
        )
      )
      .map(StructType(name, _))
}

object AquaContext extends Logging {

  trait Implicits {
    implicit val aquaContextMonoid: Monoid[AquaContext]
  }

  val blank: AquaContext =
    AquaContext(None, Set.empty, None, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  def implicits(init: AquaContext): Implicits = new Implicits {

    override implicit val aquaContextMonoid: Monoid[AquaContext] =
      new Monoid[AquaContext] {

        override def empty: AquaContext =
          init

        // TODO is it the right way?
        override def combine(x: AquaContext, y: AquaContext): AquaContext =
          AquaContext(
            x.module.orElse(y.module),
            x.declares ++ y.declares,
            x.exports
              .flatMap(xe => y.exports.map(combine(xe, _)))
              .orElse(x.exports)
              .orElse(y.exports),
            x.funcs ++ y.funcs,
            x.types ++ y.types,
            x.values ++ y.values,
            x.abilities ++ y.abilities,
            x.services ++ y.services
          )
      }
  }

  def fromServiceModel(sm: ServiceModel, serviceId: ValueModel): AquaContext =
    AquaContext(
      module = Some(sm.name),
      declares = sm.`type`.fields.toNel.map(_._1).toList.toSet,
      exports = None,
      funcs = sm.arrows.toSortedMap.map { case (fnName, arrowType) =>
        val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
        fnName ->
          FuncCallable(
            fnName,
            // TODO: capture ability resolution, get ID from the call context
            FuncOp.leaf(CallServiceTag(serviceId, fnName, call)),
            arrowType,
            ret.map(_.model),
            Map.empty,
            Map.empty
          )
      },
      types = Map.empty,
      values = Map.empty,
      abilities = Map.empty,
      services = Map.empty
    )

  def fromScriptModel(sm: ScriptModel, init: AquaContext)(implicit
    aqum: Monoid[AquaContext]
  ): AquaContext =
    sm.models
      .foldLeft((init, Monoid.empty[AquaContext])) {
        case ((ctx, exportContext), c: ConstantModel) =>
          val add =
            Monoid
              .empty[AquaContext]
              .copy(values =
                if (c.allowOverrides && ctx.values.contains(c.name)) ctx.values
                else ctx.values.updated(c.name, c.value.resolveWith(ctx.values))
              )
          (ctx |+| add, exportContext |+| add)
        case ((ctx, exportContext), func: FuncModel) =>
          val fr = func.capture(ctx.allFuncs(), ctx.allValues())
          val add =
            Monoid.empty[AquaContext].copy(funcs = ctx.funcs.updated(func.name, fr))
          (ctx |+| add, exportContext |+| add)
        case ((ctx, exportContext), t: TypeModel) =>
          val add =
            Monoid.empty[AquaContext].copy(types = ctx.types.updated(t.name, t.`type`))
          (ctx |+| add, exportContext |+| add)
        case ((ctx, exportContext), m: ServiceModel) =>
          val add =
            Monoid
              .empty[AquaContext]
              .copy(
                abilities = m.defaultId.fold(ctx.abilities)(id =>
                  ctx.abilities.updated(m.name, fromServiceModel(m, id))
                ),
                services = ctx.services.updated(m.name, m)
              )
          (ctx |+| add, exportContext |+| add)
        case (ce, _) => ce
      }
      ._2
}
