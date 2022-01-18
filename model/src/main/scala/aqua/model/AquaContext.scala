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
  funcs: Map[String, FuncArrow],
  types: Map[String, Type],
  values: Map[String, ValueModel],
  abilities: Map[String, AquaContext],
  // TODO: merge this with abilities, when have ability resolution variance
  services: Map[String, ServiceModel]
) {

  def allValues: Map[String, ValueModel] = ???

  def allFuncs: Map[String, FuncArrow] = ???

}

object AquaContext extends Logging {

  val blank: AquaContext = AquaContext(None, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

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
            FuncOp.leaf(CallServiceTag(serviceId, fnName, call)),
            arrowType,
            ret.map(_.toRaw),
            Map.empty,
            Map.empty
          )
      }
    )

  //
  def fromRawContext(rawContext: RawContext): AquaContext = {
    case class Addition(name: String, c: String => AquaContext) {
      // Only what is explicitely exported is going to be added to exports
      def toExports: AquaContext = rawContext.module.fold(c(name))(_ =>
        rawContext.exports.fold(blank)(exps => exps.get(name).fold(blank)(_.fold(c(name))(c)))
      )

      // Always add to accumulator as is
      def toAccum: AquaContext = c(name)
    }

    rawContext.parts.parts
      .foldLeft[(AquaContext, AquaContext)](
        // The first context is used to resolve imports
        // The second one is what is going to be exported
        {
          // Laziness unefficiency happens here
          (
            blank.copy(
              abilities = rawContext.abilities.view.mapValues(fromRawContext).toMap
            ),
            blank.copy(
              module = rawContext.module
            )
          )

        }
      ) {
        case ((ctx, exportContext), c: ConstantRaw) =>
          // Just saving a constant
          val add = Addition(
            c.name,
            n =>
              blank
                .copy(values =
                  if (c.allowOverrides && ctx.values.contains(c.name)) Map.empty
                  else Map(n -> ValueModel.fromRaw(c.value).resolveWith(ctx.allValues))
                )
          )

          (ctx |+| add.toAccum, exportContext |+| add.toExports)

        case ((ctx, exportContext), func: FuncRaw) =>
          // To add a function, we have to know its scope
          val fr = FuncArrow.fromRaw(func, ctx.allFuncs, ctx.allValues)
          val add = Addition(func.name, n => blank.copy(funcs = Map(n -> fr)))

          (ctx |+| add.toAccum, exportContext |+| add.toExports)

        case ((ctx, exportContext), t: TypeRaw) =>
          // Just remember the type (why?)
          val add = Addition(t.name, n => blank.copy(types = Map(n -> t.`type`)))
          (ctx |+| add.toAccum, exportContext |+| add.toExports)

        case ((ctx, exportContext), m: ServiceRaw) =>
          // To add a service, we need to resolve its ID, if any
          val id = m.defaultId.map(ValueModel.fromRaw).map(_.resolveWith(ctx.allValues))
          val srv = ServiceModel(m.name, m.arrows, id)
          val add = Addition(
            m.name,
            n =>
              blank
                .copy(
                  abilities = m.defaultId.fold(Map.empty)(id => Map(n -> fromService(m, id))),
                  services = Map(n -> srv)
                )
          )
          (ctx |+| add.toAccum, exportContext |+| add.toExports)
        case (ce, _) => ce
      }
      ._2
  }
}
