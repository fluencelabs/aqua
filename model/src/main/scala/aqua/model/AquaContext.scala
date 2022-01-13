package aqua.model

import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.{CallServiceTag, FuncOp}
import aqua.raw.value.ValueRaw
import aqua.raw.{ConstantRaw, RawContext, ServiceRaw}
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
  values: Map[String, ValueModel],
  abilities: Map[String, AquaContext],
  // TODO: merge this with abilities, when have ability resolution variance
  services: Map[String, ServiceRaw]
)

object AquaContext extends Logging {
//
//  trait Implicits {
//    implicit val aquaContextMonoid: Monoid[AquaContext]
//  }
//
//  val blank: AquaContext =
//    AquaContext(None, Set.empty, None, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
//
//  def implicits(init: AquaContext): Implicits = new Implicits {
//
//    override implicit val aquaContextMonoid: Monoid[AquaContext] =
//      new Monoid[AquaContext] {
//
//        override def empty: AquaContext =
//          init
//
//        // TODO is it the right way?
//        override def combine(x: AquaContext, y: AquaContext): AquaContext =
//          AquaContext(
//            x.module.orElse(y.module),
//            x.declares ++ y.declares,
//            x.exports
//              .flatMap(xe => y.exports.map(combine(xe, _)))
//              .orElse(x.exports)
//              .orElse(y.exports),
//            x.funcs ++ y.funcs,
//            x.types ++ y.types,
//            x.values ++ y.values,
//            x.abilities ++ y.abilities,
//            x.services ++ y.services
//          )
//      }
//  }
//
//  def fromService(sm: ServiceRaw, serviceId: ValueRaw): AquaContext =
//    AquaContext(
//      module = Some(sm.name),
//      declares = sm.`type`.fields.toNel.map(_._1).toList.toSet,
//      exports = None,
//      funcs = sm.arrows.toSortedMap.map { case (fnName, arrowType) =>
//        val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
//        fnName ->
//          FuncArrow(
//            fnName,
//            // TODO: capture ability resolution, get ID from the call context
//            FuncOp.leaf(CallServiceTag(serviceId, fnName, call)),
//            arrowType,
//            ret.map(_.toRaw),
//            Map.empty,
//            Map.empty
//          )
//      },
//      types = Map.empty,
//      values = Map.empty,
//      abilities = Map.empty,
//      services = Map.empty
//    )
//
//  def fromRawContext(rawContext: RawContext, init: AquaContext)(implicit
//    aqum: Semigroup[AquaContext]
//  ): AquaContext =
//    rawContext.parts
//      .foldLeft((init, blank)) {
//        case ((ctx, exportContext), c: ConstantRaw) =>
//          val add =
//            blank
//              .copy(values =
//                if (c.allowOverrides && ctx.values.contains(c.name)) Map.empty
//                else Map(c.name -> c.value.resolveWith(ctx.values))
//              )
//          (ctx |+| add, exportContext |+| add)
//        case ((ctx, exportContext), func: FuncRaw) =>
//          val fr = FuncArrow.fromRaw(func, ctx.allFuncs(), ctx.allValues())
//          val add =
//            blank.copy(funcs = Map(func.name -> fr))
//          (ctx |+| add, exportContext |+| add)
//        case ((ctx, exportContext), t: TypeRaw) =>
//          val add =
//            blank.copy(types = Map(t.name -> t.`type`))
//          (ctx |+| add, exportContext |+| add)
//        case ((ctx, exportContext), m: ServiceRaw) =>
//          val add =
//            blank
//              .copy(
//                abilities = m.defaultId.fold(Map.empty)(id => Map(m.name -> fromService(m, id))),
//                services = Map(m.name -> m)
//              )
//          (ctx |+| add, exportContext |+| add)
//        case (ce, _) => ce
//      }
//      ._2
}
