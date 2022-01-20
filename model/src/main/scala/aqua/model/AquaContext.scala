package aqua.model

import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.{CallServiceTag, FuncOp}
import aqua.raw.value.ValueRaw
import aqua.raw.{ConstantRaw, RawContext, ServiceRaw, TypeRaw}
import aqua.types.{StructType, Type}
import cats.Monoid
import cats.data.NonEmptyMap
import cats.kernel.Semigroup
import cats.syntax.functor.*
import cats.syntax.monoid.*
import scribe.Logging
import aqua.model.inline.ArgsCall

import scala.collection.immutable.SortedMap

case class AquaContext(
  module: Option[String],
  exports: Map[String, Option[String]],
  funcs: Map[String, FuncArrow],
  types: Map[String, Type],
  values: Map[String, ValueModel],
  abilities: Map[String, AquaContext],
  // TODO: merge this with abilities, when have ability resolution variance
  services: Map[String, ServiceModel]
) {

  private def prefixFirst[T](prefix: String, pair: (String, T)): (String, T) =
    (prefix + pair._1, pair._2)

  // TODO: it's a duplicate
  private def all[T](what: AquaContext => Map[String, T], prefix: String = ""): Map[String, T] =
    abilities
      .foldLeft(what(this)) { case (ts, (k, v)) =>
        ts ++ v.all(what, k + ".")
      }
      .map(prefixFirst(prefix, _))

  lazy val allValues: Map[String, ValueModel] =
    all(_.values)

  lazy val allFuncs: Map[String, FuncArrow] =
    all(_.funcs)

  // TODO remove this ugliness
  def pick(name: String, maybeRename: Option[String]): AquaContext =
    funcs
      .get(name)
      .fold(AquaContext.blank)(p =>
        AquaContext.blank.copy(funcs = Map(maybeRename.getOrElse(name) -> p))
      ) |+|
      types
        .get(name)
        .fold(AquaContext.blank)(p =>
          AquaContext.blank.copy(types = Map(maybeRename.getOrElse(name) -> p))
        ) |+|
      values
        .get(name)
        .fold(AquaContext.blank)(p =>
          AquaContext.blank.copy(values = Map(maybeRename.getOrElse(name) -> p))
        ) |+|
      abilities
        .get(name)
        .fold(AquaContext.blank)(p =>
          AquaContext.blank.copy(abilities = Map(maybeRename.getOrElse(name) -> p))
        ) |+|
      services
        .get(name)
        .fold(AquaContext.blank)(p =>
          AquaContext.blank.copy(services = Map(maybeRename.getOrElse(name) -> p))
        )

  lazy val exported: AquaContext =
    exports.foldLeft(AquaContext.blank.copy(module = module)) { case (acc, (k, v)) =>
      acc |+| pick(k, v)
    }
}

object AquaContext extends Logging {

  val blank: AquaContext =
    AquaContext(None, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  given Monoid[AquaContext] with

    val empty: AquaContext =
      blank

    def combine(x: AquaContext, y: AquaContext): AquaContext =
      AquaContext(
        x.module orElse y.module,
        x.exports ++ y.exports,
        x.funcs ++ y.funcs,
        x.types ++ y.types,
        x.values ++ y.values,
        x.abilities ++ y.abilities,
        x.services ++ y.services
      )

  //
  def fromService(sm: ServiceRaw, serviceId: ValueRaw): AquaContext =
    blank.copy(
      module = Some(sm.name),
      funcs = sm.arrows.toSortedMap.map { case (fnName, arrowType) =>
        val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
        fnName ->
          FuncArrow(
            fnName,
            // TODO: capture ability resolution, get ID from the call context
            FuncOp.leaf(CallServiceTag(serviceId, fnName, call)),
            arrowType,
            ret.map(_.toRaw),
            Map.empty,
            Map.empty
          )
      }
    )

  def fromRaw(rawContext: RawContext): AquaContext = fromRawContext(rawContext)._2

  //
  private def fromRawContext(rawContext: RawContext): (AquaContext, AquaContext) =
    rawContext.parts.parts
      .foldLeft[(AquaContext, AquaContext)](
        // The first context is used to resolve imports
        // The second one is what is going to be exported
        {
          // Laziness unefficiency happens here
          (
            rawContext.init
              .map(fromRawContext(_)._1)
              .getOrElse(
                blank.copy(
                  abilities = rawContext.abilities.view.mapValues(fromRawContext(_)._2).toMap
                )
              ),
            rawContext.init
              .map(fromRawContext(_)._2)
              // Handle reexports
              .map(_.copy(exports = rawContext.exports.getOrElse(Map.empty)).exported)
              .getOrElse(blank)
              .copy(
                module = rawContext.module,
                exports = rawContext.exports.getOrElse(Map.empty)
              )
          )

        }
      ) {
        case ((ctx, exportContext), c: ConstantRaw) =>
          // Just saving a constant
          val add =
            blank
              .copy(values =
                if (c.allowOverrides && ctx.values.contains(c.name)) Map.empty
                else Map(c.name -> ValueModel.fromRaw(c.value).resolveWith(ctx.allValues))
              )

          (ctx |+| add, exportContext |+| add)

        case ((ctx, exportContext), func: FuncRaw) =>
          // To add a function, we have to know its scope
          logger.trace(s"values for ${func.name} at ${rawContext.module} " + ctx.allValues.keySet)
          val fr = FuncArrow.fromRaw(func, ctx.allFuncs, ctx.allValues)
          val add = blank.copy(funcs = Map(func.name -> fr))

          (ctx |+| add, exportContext |+| add)

        case ((ctx, exportContext), t: TypeRaw) =>
          // Just remember the type (why?)
          val add = blank.copy(types = Map(t.name -> t.`type`))
          (ctx |+| add, exportContext |+| add)

        case ((ctx, exportContext), m: ServiceRaw) =>
          // To add a service, we need to resolve its ID, if any
          val id = m.defaultId.map(ValueModel.fromRaw).map(_.resolveWith(ctx.allValues))
          val srv = ServiceModel(m.name, m.arrows, id)
          val add =
            blank
              .copy(
                abilities = m.defaultId.fold(Map.empty)(id => Map(m.name -> fromService(m, id))),
                services = Map(m.name -> srv)
              )

          (ctx |+| add, exportContext |+| add)
        case (ce, _) => ce
      }

}
