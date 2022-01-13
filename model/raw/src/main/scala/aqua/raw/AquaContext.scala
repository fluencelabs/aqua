package aqua.raw

import aqua.raw.arrow.{ArgsCall, FuncArrow, FuncRaw}
import aqua.raw.ops.{CallServiceTag, FuncOp}
import aqua.raw.value.ValueRaw
import aqua.types.{StructType, Type}
import cats.Monoid
import cats.data.NonEmptyMap
import cats.kernel.Semigroup
import cats.syntax.functor.*
import cats.syntax.monoid.*
import scribe.Logging

import scala.collection.immutable.SortedMap

case class AquaContext(
                        module: Option[String],
                        declares: Set[String],
                        exports: Option[AquaContext],
                        funcs: Map[String, FuncArrow],
                        types: Map[String, Type],
                        values: Map[String, ValueRaw],
                        abilities: Map[String, AquaContext],
                        // TODO: merge this with abilities, when have ability resolution variance
                        services: Map[String, ServiceRaw]
) {

  private def prefixFirst[T](prefix: String, pair: (String, T)): (String, T) =
    (prefix + pair._1, pair._2)

  def isEmpty: Boolean = this == AquaContext.blank
  def nonEmpty: Boolean = !isEmpty

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
      .filter(_.nonEmpty)

  def pickHeader: AquaContext =
    AquaContext.blank.copy(module = module, declares = declares, exports = exports)

  def pickDeclared(implicit semi: Semigroup[AquaContext]): AquaContext =
    if (module.isEmpty) this
    else
      declares.toList
        .flatMap(pick(_, None))
        .foldLeft(pickHeader)(
          _ |+| _
        )

  def allTypes(prefix: String = ""): Map[String, Type] =
    abilities
      .foldLeft(types) { case (ts, (k, v)) =>
        ts ++ v.allTypes(k + ".")
      }
      .map(prefixFirst(prefix, _))

  def allFuncs(prefix: String = ""): Map[String, FuncArrow] =
    abilities
      .foldLeft(funcs) { case (ts, (k, v)) =>
        ts ++ v.allFuncs(k + ".")
      }
      .map(prefixFirst(prefix, _))

  def allValues(prefix: String = ""): Map[String, ValueRaw] =
    abilities
      .foldLeft(values) { case (ts, (k, v)) =>
        ts ++ v.allValues(k + ".")
      }
      .map(prefixFirst(prefix, _))

  def allServices(prefix: String = ""): Map[String, ServiceRaw] =
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
            // TODO do we need to pass abilities as type?
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

  def fromService(sm: ServiceRaw, serviceId: ValueRaw): AquaContext =
    AquaContext(
      module = Some(sm.name),
      declares = sm.`type`.fields.toNel.map(_._1).toList.toSet,
      exports = None,
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
      },
      types = Map.empty,
      values = Map.empty,
      abilities = Map.empty,
      services = Map.empty
    )

  def fromRawContext(rawContext: ContextRaw, init: AquaContext)(implicit
    aqum: Semigroup[AquaContext]
  ): AquaContext =
    rawContext.parts
      .foldLeft((init, blank)) {
        case ((ctx, exportContext), c: ConstantRaw) =>
          val add =
            blank
              .copy(values =
                if (c.allowOverrides && ctx.values.contains(c.name)) Map.empty
                else Map(c.name -> c.value.resolveWith(ctx.values))
              )
          (ctx |+| add, exportContext |+| add)
        case ((ctx, exportContext), func: FuncRaw) =>
          val fr = func.capture(ctx.allFuncs(), ctx.allValues())
          val add =
            blank.copy(funcs = Map(func.name -> fr))
          (ctx |+| add, exportContext |+| add)
        case ((ctx, exportContext), t: TypeRaw) =>
          val add =
            blank.copy(types = Map(t.name -> t.`type`))
          (ctx |+| add, exportContext |+| add)
        case ((ctx, exportContext), m: ServiceRaw) =>
          val add =
            blank
              .copy(
                abilities = m.defaultId.fold(Map.empty)(id => Map(m.name -> fromService(m, id))),
                services = Map(m.name -> m)
              )
          (ctx |+| add, exportContext |+| add)
        case (ce, _) => ce
      }
      ._2
}
