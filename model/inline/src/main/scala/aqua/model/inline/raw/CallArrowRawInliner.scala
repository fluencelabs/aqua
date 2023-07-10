package aqua.model.inline.raw

import aqua.model.inline.Inline.parDesugarPrefixOpt
import aqua.model.{CallServiceModel, FuncArrow, MetaModel, SeqModel, ValueModel, VarModel}
import aqua.model.inline.{ArrowInliner, Inline, TagInliner}
import aqua.model.inline.RawValueInliner.{callToModel, valueToModel}
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.ops.Call
import aqua.types.ArrowType
import aqua.raw.value.CallArrowRaw
import cats.data.{Chain, State}
import scribe.Logging

import scala.collection.immutable.ListMap

object CallArrowRawInliner extends RawInliner[CallArrowRaw] with Logging {

  private[inline] def unfoldArrow[S: Mangler: Exports: Arrows](
    value: CallArrowRaw,
    exportTo: List[Call.Export]
  ): State[S, (List[ValueModel], Inline)] = Exports[S].exports.flatMap { exports =>
    logger.trace(s"${exportTo.mkString(" ")} $value")

    val call = Call(value.arguments, exportTo)
    value.serviceId match {
      case Some(serviceId) =>
        logger.trace(Console.BLUE + s"call service id $serviceId" + Console.RESET)
        for {
          cd <- callToModel(call, true)
          sd <- valueToModel(serviceId)
        } yield cd._1.exportTo.map(_.asVar.resolveWith(exports)) -> Inline(
          ListMap.empty,
          Chain(
            SeqModel.wrap(
              sd._2.toList ++
                cd._2.toList :+ CallServiceModel(sd._1, value.name, cd._1).leaf: _*
            )
          )
        )
      case None =>
        /**
         * Here the back hop happens from [[TagInliner]] to [[ArrowInliner.callArrow]]
         */
        val funcName = value.ability.fold(value.name)(_ + "." + value.name)
        logger.trace(s"            $funcName")

        resolveArrow(funcName, call)
    }
  }

  private def resolveFuncArrow[S: Mangler: Exports: Arrows](
    fn: FuncArrow,
    call: Call
  ): State[S, (List[ValueModel], Inline)] = {
    logger.trace(Console.YELLOW + s"Call arrow ${fn.funcName}" + Console.RESET)
    callToModel(call, false).flatMap { case (cm, p) =>
      ArrowInliner
        .callArrowRet(fn, cm)
        .map { case (body, vars) =>
          vars -> Inline(
            ListMap.empty,
            Chain.one(
              // Leave meta information in tree after inlining
              MetaModel
                .CallArrowModel(fn.funcName)
                .wrap(
                  SeqModel.wrap(p.toList :+ body: _*)
                )
            )
          )
        }
    }
  }

  private def resolveArrow[S: Mangler: Exports: Arrows](
    funcName: String,
    call: Call
  ): State[S, (List[ValueModel], Inline)] = for {
    arrows <- Arrows[S].arrows
    exports <- Exports[S].exports
    arrow = arrows
      .get(funcName)
      .orElse(
        // if there is no arrow, check if it is stored in Exports as variable and try to resolve it
        exports
          .get(funcName)
          .collect { case VarModel(name, _: ArrowType, _) =>
            name
          }
          .flatMap(arrows.get)
      )
    result <- arrow.fold {
      logger.error(
        s"Inlining, cannot find arrow $funcName, available: ${arrows.keys
          .mkString(", ")} and vars: ${exports.keys.mkString(", ")}"
      )

      State.pure(Nil -> Inline.empty)
    }(resolveFuncArrow(_, call))
  } yield result

  override def apply[S: Mangler: Exports: Arrows](
    raw: CallArrowRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    Mangler[S]
      .findAndForbidName(raw.name)
      .flatMap(n =>
        unfoldArrow(raw, Call.Export(n, raw.`type`) :: Nil).map {
          case (Nil, inline) => (VarModel(n, raw.`type`), inline)
          case (h :: _, inline) => (h, inline)
        }
      )
}
