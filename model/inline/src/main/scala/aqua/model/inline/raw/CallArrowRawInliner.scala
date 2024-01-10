package aqua.model.inline.raw

import aqua.errors.Errors.internalError
import aqua.model.*
import aqua.model.inline.RawValueInliner.callToModel
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.inline.{ArrowInliner, Inline, RawValueInliner}
import aqua.raw.ops.Call
import aqua.raw.value.CallArrowRaw

import cats.data.{Chain, State}
import cats.syntax.traverse.*
import scribe.Logging

object CallArrowRawInliner extends RawInliner[CallArrowRaw] with Logging {

  private[inline] def unfold[S: Mangler: Exports: Arrows](
    value: CallArrowRaw,
    exportTo: List[Call.Export]
  ): State[S, (List[ValueModel], Inline)] = {
//    logger.trace(s"${exportTo.mkString(" ")} $value")

    val call = Call(value.arguments, exportTo)

    /**
     * Here the back hop happens from [[TagInliner]] to [[ArrowInliner.callArrow]]
     */
    val funcName = value.ability.fold(value.name)(_ + "." + value.name)
//    logger.trace(s"            $funcName")

    resolveArrow(funcName, call)
  }

  private def resolveFuncArrow[S: Mangler: Exports: Arrows](
    fn: FuncArrow,
    call: Call
  ): State[S, (List[ValueModel], Inline)] = {
//    logger.trace(Console.YELLOW + s"Call arrow ${fn.funcName}" + Console.RESET)
    callToModel(call, false).flatMap { case (cm, p) =>
      ArrowInliner
        .callArrowRet(fn, cm)
        .map { case (body, vars) =>
          vars -> Inline(
            Chain.one(
              // Leave meta information in tree after inlining
              MetaModel
                .CallArrowModel(fn.funcName)
                .wrap(SeqModel.wrap(p.toList :+ body))
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
    lastArrow <- Exports[S].getLastVarName(funcName)
    arrow = arrows
      .get(funcName)
      .orElse(
        // if there is no arrow, check if it is stored in Exports as variable and try to resolve it
        lastArrow.flatMap(arrows.get)
      )
    result <- arrow
      .traverse(resolveFuncArrow(_, call))
      .map(_.getOrElse {
        val arrs = arrows.keys.mkString(", ")
        val vars = exports.keys.mkString(", ")
        internalError(
          s"Inlining, cannot find arrow ($funcName), available: ($arrs) and vars: ($vars)"
        )
      })
  } yield result

  override def apply[S: Mangler: Exports: Arrows](
    raw: CallArrowRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    Mangler[S]
      .findAndForbidName(raw.name)
      .flatMap(n =>
        unfold(raw, Call.Export(n, raw.`type`) :: Nil).map {
          case (Nil, inline) => (VarModel(n, raw.`type`), inline)
          case (h :: _, inline) => (h, inline)
        }
      )
}
