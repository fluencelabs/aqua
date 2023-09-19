package aqua.model

import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.CallArrowRawTag
import aqua.raw.value.ValueRaw
import aqua.raw.value.CallArrowRaw
import aqua.raw.{ConstantRaw, RawContext, RawPart, ServiceRaw, TypeRaw}
import aqua.types.{AbilityType, StructType, Type}

import cats.Monoid
import cats.data.NonEmptyMap
import cats.data.Chain
import cats.kernel.Semigroup
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.syntax.bifunctor.*
import cats.syntax.monoid.*
import cats.syntax.option.*
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

  // TODO: it's a duplicate
  private def all[T](
    what: AquaContext => Map[String, T],
    prefix: String = ""
  ): Map[String, T] = (
    what(this) ++ abilities.toList.foldMap { case (k, v) =>
      v.all(what, k + ".").toList
    }
  ).map(_.leftMap(prefix + _)).toMap

  lazy val allValues: Map[String, ValueModel] =
    all(_.values) ++
      /**
       * Add values from services that have default ID
       * So that they will be available in functions.
       */
      services.flatMap { case (srvName, srv) =>
        srv.defaultId.toList.flatMap(_ =>
          srv.`type`.arrows.map { case (arrowName, arrowType) =>
            val fullName = AbilityType.fullName(srvName, arrowName)

            fullName -> VarModel(
              fullName,
              arrowType
            )
          }.updated(
            srvName,
            VarModel(
              srvName,
              srv.`type`
            )
          )
        )
      }

  lazy val allFuncs: Map[String, FuncArrow] =
    all(_.funcs) ++
      /**
       * Add functions from services that have default ID
       * So that they will be available in functions.
       */
      services.flatMap { case (srvName, srv) =>
        srv.defaultId.toList.flatMap(id =>
          srv.`type`.arrows.map { case (arrowName, arrowType) =>
            val fullName = AbilityType.fullName(srvName, arrowName)

            fullName -> FuncArrow.fromServiceMethod(
              fullName,
              srvName,
              arrowName,
              arrowType,
              id
            )
          }
        )
      }

  private def pickOne[T](
    name: String,
    newName: String,
    ctx: Map[String, T],
    add: (AquaContext, Map[String, T]) => AquaContext
  ): AquaContext = ctx
    .get(name)
    .fold(AquaContext.blank)(t =>
      add(
        AquaContext.blank,
        Map(newName -> t)
      )
    )

  def pick(name: String, maybeRename: Option[String]): AquaContext = {
    val newName = maybeRename.getOrElse(name)
    pickOne(name, newName, funcs, (ctx, el) => ctx.copy(funcs = el)) |+|
      pickOne(name, newName, types, (ctx, el) => ctx.copy(types = el)) |+|
      pickOne(name, newName, values, (ctx, el) => ctx.copy(values = el)) |+|
      pickOne(name, newName, abilities, (ctx, el) => ctx.copy(abilities = el)) |+|
      pickOne(name, newName, services, (ctx, el) => ctx.copy(services = el))
  }
}

object AquaContext extends Logging {

  case class Cache(private val data: Chain[(RawContext, AquaContext)] = Chain.empty) {
    lazy val size: Long = data.size

    def get(ctx: RawContext): Option[AquaContext] =
      data.collectFirst { case (rawCtx, aquaCtx) if rawCtx eq ctx => aquaCtx }

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

  def fromService(sm: ServiceRaw, serviceId: ValueRaw): AquaContext =
    blank.copy(
      module = Some(sm.name),
      funcs = sm.`type`.arrows.map { case (fnName, arrowType) =>
        val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
        fnName ->
          FuncArrow(
            fnName,
            // TODO: capture ability resolution, get ID from the call context
            CallArrowRawTag.service(serviceId, fnName, call, sm.name).leaf,
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

    rawContext.exports
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
              val id = m.defaultId
                .map(ValueModel.fromRaw)
                .map(_.resolveWith(pctx.allValues))
              val srv = ServiceModel(m.name, m.`type`, id)
              val add =
                blank
                  .copy(
                    abilities = m.defaultId
                      .map(id => Map(m.name -> fromService(m, id)))
                      .orEmpty,
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
