package aqua.model

import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.{CallArrowRawTag, FuncOp}
import aqua.raw.value.ValueRaw
import aqua.raw.value.CallArrowRaw
import aqua.raw.{ConstantRaw, RawContext, RawPart, ServiceRaw, TypeRaw}
import aqua.types.{StructType, Type}
import cats.Monoid
import cats.data.NonEmptyMap
import cats.data.Chain
import cats.kernel.Semigroup
import cats.syntax.functor.*
import cats.syntax.monoid.*
import scribe.Logging

import scala.collection.immutable.SortedMap

case class AquaContext(
  module: Option[String],
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
}

object AquaContext extends Logging {

  case class Cache(private val data: Chain[(RawContext, AquaContext)] = Chain.empty) {
    lazy val size: Long = data.size

    def get(ctx: RawContext): Option[AquaContext] =
      data.find(_._1 eq ctx).map(_._2)

    def updated(ctx: RawContext, aCtx: AquaContext): Cache = copy(data :+ (ctx -> aCtx))
  }

  val blank: AquaContext =
    AquaContext(None, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  given Monoid[AquaContext] with

    val empty: AquaContext =
      blank

    def combine(x: AquaContext, y: AquaContext): AquaContext =
      AquaContext(
        x.module orElse y.module,
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
            CallArrowRawTag.service(sm.name, serviceId, fnName, call).leaf,
            arrowType,
            ret.map(_.toRaw),
            Map.empty,
            Map.empty,
            None
          )
      }
    )

  // Convert RawContext into AquaContext, with exports handled
  def exportsFromRaw(rawContext: RawContext, cache: Cache): (AquaContext, Cache) = {
    logger.trace(s"ExportsFromRaw ${rawContext.module}")
    val (ctx, newCache) = fromRawContext(rawContext, cache)
    logger.trace("raw: " + rawContext)
    logger.trace("ctx: " + ctx)

    rawContext.module
      .fold(
        // if `module` header is not defined, then export everything defined in rawContext
        rawContext.parts.map(_._2).map(_.name).map(_ -> Option.empty[String]).toList.toMap
      )(_ => rawContext.exports.getOrElse(Map.empty))
      .foldLeft(
        // Module name is what persists
        blank.copy(
          module = ctx.module
        )
      ) { case (acc, (k, v)) =>
        // Pick exported things, accumulate
        acc |+| ctx.pick(k, v)
      } -> newCache
  }

  // Convert RawContext into AquaContext, with no exports handled
  private def fromRawContext(rawContext: RawContext, cache: Cache): (AquaContext, Cache) =
    cache
      .get(rawContext)
      .fold {
        logger.trace(s"Compiling ${rawContext.module}, cache has ${cache.size} entries")

        val (newCtx, newCache) = rawContext.parts
          .foldLeft[(AquaContext, Cache)] {
            // Laziness unefficiency happens here
            logger.trace(s"raw: ${rawContext.module}")
            val (i, c) =
              rawContext.init
                .map(fromRawContext(_, cache))
                .getOrElse(blank -> cache)

            val (abs, absCache) =
              rawContext.abilities.foldLeft[(Map[String, AquaContext], Cache)]((Map.empty, c)) {
                case ((acc, cAcc), (k, v)) =>
                  val (abCtx, abCache) = fromRawContext(v, cAcc)
                  (acc + (k -> abCtx), abCache)
              }

            (i |+| blank.copy(abilities = abs)) -> absCache
          } {
            case ((ctx, ctxCache), (partContext, c: ConstantRaw)) =>
              logger.trace("Adding constant " + c.name)
              // Just saving a constant
              // Actually this should have no effect, as constants are resolved by semantics
              val (pctx, pcache) = fromRawContext(partContext, ctxCache)
              logger.trace("Got " + c.name + " from raw")
              val add =
                blank
                  .copy(values =
                    if (c.allowOverrides && pctx.values.contains(c.name)) Map.empty
                    else Map(c.name -> ValueModel.fromRaw(c.value).resolveWith(pctx.allValues))
                  )

              (ctx |+| add, pcache)

            case ((ctx, ctxCache), (partContext, func: FuncRaw)) =>
              // To add a function, we have to know its scope
              logger.trace("Adding func " + func.name)

              val (pctx, pcache) = fromRawContext(partContext, ctxCache)
              logger.trace("Got " + func.name + " from raw")
              val fr = FuncArrow.fromRaw(func, pctx.allFuncs, pctx.allValues, None)
              logger.trace("Captured recursively for " + func.name)
              val add = blank.copy(funcs = Map(func.name -> fr))

              (ctx |+| add, pcache)

            case ((ctx, ctxCache), (_, t: TypeRaw)) =>
              // Just remember the type (why? it's can't be exported, so seems useless)
              val add = blank.copy(types = Map(t.name -> t.`type`))
              (ctx |+| add, ctxCache)

            case ((ctx, ctxCache), (partContext, m: ServiceRaw)) =>
              // To add a service, we need to resolve its ID, if any
              logger.trace("Adding service " + m.name)
              val (pctx, pcache) = fromRawContext(partContext, ctxCache)
              logger.trace("Got " + m.name + " from raw")
              val id = m.defaultId.map(ValueModel.fromRaw).map(_.resolveWith(pctx.allValues))
              val srv = ServiceModel(m.name, m.arrows, id)
              val add =
                blank
                  .copy(
                    abilities =
                      m.defaultId.fold(Map.empty)(id => Map(m.name -> fromService(m, id))),
                    services = Map(m.name -> srv)
                  )

              (ctx |+| add, pcache)
            case (ctxAndCache, _) => ctxAndCache
          }

        (newCtx, newCache.updated(rawContext, newCtx))

      } { ac =>
        logger.trace("Got from cache")
        ac -> cache
      }

}
