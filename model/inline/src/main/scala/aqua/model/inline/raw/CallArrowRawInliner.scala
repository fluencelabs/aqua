package aqua.model.inline.raw

import aqua.model.inline.Inline.parDesugarPrefixOpt
import aqua.model.{CallServiceModel, ValueModel, VarModel}
import aqua.model.inline.{ArrowInliner, Inline, TagInliner}
import aqua.model.inline.RawValueInliner.{callToModel, valueToModel}
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.ops.Call
import aqua.raw.value.CallArrowRaw
import cats.data.{Chain, State}
import scribe.Logging

object CallArrowRawInliner extends RawInliner[CallArrowRaw] with Logging {

  private[inline] def unfoldArrow[S: Mangler: Exports: Arrows](
    value: CallArrowRaw,
    exportTo: List[Call.Export]
  ): State[S, (List[ValueModel], Inline)] = {
    val call = Call(value.arguments, exportTo)
    value.serviceId match {
      case Some(serviceId) =>
        for {
          cd <- callToModel(call)
          sd <- valueToModel(serviceId)
        } yield cd._1.exportTo.map(_.asVar) -> Inline(
          Map.empty,
          Chain.fromOption(
            parDesugarPrefixOpt(
              sd._2,
              cd._2
            )
          ) :+ CallServiceModel(sd._1, value.name, cd._1).leaf
        )
      case None =>
        /**
         * Here the back hop happens from [[TagInliner]] to [[ArrowInliner.callArrow]]
         */
        val funcName = value.ability.fold(value.name)(_ + "." + value.name)
        logger.trace(s"            $funcName")
        Arrows[S].arrows.flatMap(arrows =>
          arrows.get(funcName) match {
            case Some(fn) =>
              logger.trace(s"Call arrow $funcName")
              callToModel(call).flatMap { case (cm, p) =>
                ArrowInliner
                  .callArrow(fn, cm)
                  .map(body =>
                    cm.exportTo.map(_.asVar) -> Inline(Map.empty, Chain.fromSeq(p.toList :+ body))
                  )
              }
            case None =>
              logger.error(
                s"Inlining, cannot find arrow ${funcName}, available: ${arrows.keys
                  .mkString(", ")}"
              )
              State.pure(Nil -> Inline.empty)
          }
        )
    }
  }

  override def apply[S: Mangler: Exports: Arrows](
    raw: CallArrowRaw,
    lambdaAllowed: Boolean
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
